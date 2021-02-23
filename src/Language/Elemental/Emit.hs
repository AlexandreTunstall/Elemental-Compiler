{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Functions to convert an Elemental program into an LLVM module.
module Language.Elemental.Emit
    ( emitProgram
    ) where

import Control.Arrow (Arrow((***)), (>>>))
import Control.Effect.Choose (Choose, (<|>))
import Control.Effect.Empty (Empty, empty)
import Control.Monad (void, (>=>))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text.Short (toShortByteString)
import Data.Traversable (for)
import Data.Word (Word32)
import LLVM.AST (Definition, Operand, Terminator(Br, CondBr, Ret))
import LLVM.AST qualified as LLVM
import LLVM.AST.CallingConvention qualified as LLVM.CallConv
import LLVM.AST.Constant qualified as LLVM.Constant

import Control.Carrier.ModuleBuilder
import Control.Effect.IRBuilder
import Language.Elemental.Emit.Internal
import Language.Elemental.Normalise
import Language.Elemental.Primitive
import Language.Elemental.Rewrite
import Language.Elemental.Syntax.Internal hiding ((:$), (:@), (:\), (:->))
import Language.Elemental.Syntax.Pretty


-- | Converts an Elemental program into a list of LLVM definitions.
emitProgram :: Has (Rewriter (Expr a)) sig m => Program a -> m [Definition]
emitProgram p = runModuleBuilder (const . pure) emptyModuleBuilder
    $ inlinePrimitives p >>= normaliseProgram >>= traverse emitDecl . getDecls
  where
    getDecls :: Program a -> [Decl a]
    getDecls (Program _ decls) = decls
{-# INLINABLE emitProgram #-}

-- | Emits an Elemental declaration.
emitDecl
    :: (Has ModuleBuilder sig m, Has (Rewriter (Expr a)) sig m)
    => Decl a -> m ()
emitDecl = \case
    Binding {} -> pure ()
    ForeignImport {} -> error "emitDecl: unexpected foreign import"
    ForeignExport l foreignName expr t -> do
        let (argTys, retTy) = arrowToLlvmType t
            targs = fst $ splitArrow t
        void . function (LLVM.Name $ toShortByteString foreignName) argTys retTy
            $ applyArgs l expr targs . reverse >=> emitExpr
    ForeignPrimitive {} -> error "emitDecl: unexpected foreign primitive"
    ForeignAddress {} -> error "emitDecl: unexpected foreign address"
  where
    applyArgs
        :: (Has IRBuilder sig m, Has (Rewriter (Expr a)) sig m)
        => a -> Expr a -> [Type a] -> [Operand] -> m (Expr a)
    applyArgs l ex targs ops = normaliseLlvm $ foldr (flip $ App l) ex
        $ zipWith (marshall l) targs ops

    marshall :: a -> Type a -> Operand -> Expr a
    marshall l t op = App l (marshallIn l t) . InternalExpr l $ LlvmOperand op

-- | Emits an expression with a return instruction and starts a new basic block.
emitExpr
    :: (Has IRBuilder sig m, Has (Rewriter (Expr a)) sig m) => Expr a -> m ()
emitExpr expr = do
    mop <- emitExprBlock expr
    emitTerm $ Ret mop []
    void block

-- | Emits a basic block for an expression.
emitExprBlock
    :: (Has IRBuilder sig m, Has (Rewriter (Expr a)) sig m)
    => Expr a -> m (Maybe LLVM.Operand)
emitExprBlock expr = case expr of
    InternalExpr _ Unit -> pure Nothing
    InternalExpr _ (LlvmOperand op) -> pure $ pure op
    InternalExpr _ (Emit m) -> m >>= emitExprBlock
    _ -> error $ "emitExprBlock: illegal expression: "
        <> show (prettyExpr0 expr)

-- | Converts primitives and foreign imports into the corresponding bindings.
inlinePrimitives :: Has ModuleBuilder sig m => Program a -> m (Program a)
inlinePrimitives (Program l decls) = Program l <$> traverse go decls
  where
    go :: Has ModuleBuilder sig m => Decl a -> m (Decl a)
    go decl = case decl of
        Binding {} -> pure decl
        ForeignImport l' name foreignName t -> Binding l' name <$> do
            let (targs, tret) = splitArrow t
                ltargs = toArgumentType <$> targs
                ltret = toReturnType tret
            func <- extern (LLVM.Name $ toShortByteString foreignName)
                ltargs ltret
            pure $ foldr (Lam l') (foldr (flip $ App l')
                (InternalExpr l' $ Call func (length ltargs) ltret)
                $ zipWith (genMarshall l') [0..] targs
                ) targs
        ForeignExport {} -> pure decl
        ForeignPrimitive l' dname@(DeclName _ name) _ -> pure . Binding l' dname
            . (<$) l' . snd $ primitives M.! name
        ForeignAddress l' dname addr t -> pure . Binding l' dname
            . InternalExpr l' . LlvmOperand . LLVM.ConstantOperand
            {-
                With GHC 9, it would be sounder to work out the needed bit size
                using the base 2 logarithm. Although it can be done with older
                GHC versions, differences between the two integer libraries make
                it necessary to test on both libraries.

                Until GHC 9, 128 bits should work for most architectures. Using
                a smaller size than the architecture may truncate the address.
            -}
            . LLVM.Constant.IntToPtr (LLVM.Constant.Int 128 addr)
            $ toPointerType t

    genMarshall :: a -> Int -> Type a -> Expr a
    genMarshall l' idx t = App l' (marshallOut l' t) (Var l' idx)

{-|
    Normalises an expression. Unlike 'normaliseExpr', this has additional
    rewrite rules to normalise internal expressions.
-}
normaliseLlvm
    :: (Has IRBuilder sig m, Has (Rewriter (Expr a)) sig m)
    => Expr a -> m (Expr a)
normaliseLlvm = beginRewrite (rewriteExpr $ rewriteLlvm normaliseLlvm)
    >=> joinEmit
  where
    joinEmit
        :: (Has IRBuilder sig m, Has (Rewriter (Expr a)) sig m)
        => Expr a -> m (Expr a)
    joinEmit ex = case ex of
        InternalExpr _ (Emit ey) -> ey >>= normaliseLlvm
        _ -> pure ex

-- Pattern synonyms so that the rewriter is readable.

-- | 'App' synonym
pattern (:$) :: Expr () -> Expr () -> Expr ()
pattern ef :$ ex = App () ef ex
infixl 1 :$

-- | 'TypeApp' synonym
pattern (:@) :: Expr () -> Type () -> Expr ()
pattern ef :@ tx = TypeApp () ef tx
infixl 1 :@

-- | 'Lam' synonym
pattern (:\) :: Type () -> Expr () -> Expr ()
pattern tx :\ ey = Lam () tx ey
infixr 0 :\

-- | v'Arrow' synonym
pattern (:->) :: Type () -> Type () -> Type ()
pattern tx :-> ty = Arrow () tx ty
infixr 2 :->

-- | 'Var' synonym
pattern V :: Int -> Expr ()
pattern V idx = Var () idx

-- | 'TypeVar' synonym
pattern TV :: Int -> Type ()
pattern TV idx = TypeVar () idx

-- | 'TypeLam' synonym
pattern TL :: Expr () -> Expr ()
pattern TL ey = TypeLam () ey

-- | 'Forall' synonym
pattern FA :: Type () -> Type ()
pattern FA ty = Forall () ty

-- | 'IOType' synonym
pattern IOT :: Type () -> Type ()
pattern IOT tx = SpecialType () (IOType () tx)

-- | 'PointerType' synonym
pattern PtrT :: PointerKind -> Type () -> Type ()
pattern PtrT pk tx = SpecialType () (PointerType () pk tx)

-- | v'InternalExpr' synonym
pattern IE :: InternalExpr () -> Expr ()
pattern IE ie = InternalExpr () ie

-- | v'InternalType' synonym
pattern IT :: InternalType () -> Type ()
pattern IT it = SpecialType () (InternalType () it)

{-|
    Rewrites an expression into a lower-level expression.
    This is used to fold an expression containing LLVM symbols into
    either 'Unit' or an 'LlvmOperand' (either of which may be nested in 'Emit').
-}
rewriteLlvm
    :: forall sig m a. (Has Choose sig m, Has Empty sig m)
    => (forall sig' n b. (Has IRBuilder sig' n,  Has (Rewriter (Expr b)) sig' n)
        => Expr b -> n (Expr b))
    -> Expr a -> m (Expr a)
rewriteLlvm normExpr ea = reassociate ea <|> llvmPure ea <|> llvmBind ea
    <|> llvmBits ea <|> withLabel llvmBitVector ea
    <|> withLabel (llvmCall []) ea <|> llvmPointer ea <|> liftEmit ea
  where
    withLabel :: (Functor f, Labelled f) => (f () -> m (f ())) -> f a -> m (f a)
    withLabel f ex = fmap (getLabel ex <$) . f $ () <$ ex

    {-
        Rules that could be worth adding:
        - #isolate#i# < ... >
    -}
    reassociate :: Expr a -> m (Expr a)
    reassociate = withLabel $ \case
        -- bindIO @t (pureIO @t x) = Λ λ(t → IO 0) 0 x
        IE BindIO :@ _ :$ (IE PureIO :@ t :$ e) -> pure . TL
            $ t :-> IOT (TV 0) :\ V 0 :$ e
        -- #testBit b @(t1 → t2) x y = λt1 #testBit b @t2 (x[+/0] 0) (y[+/0] 0)
        IE TestBit :$ eb :@ tx :-> ty :$ ex :$ ey -> pure
            $ tx :\ IE TestBit :$ eb :@ ty
            :$ (incrementExpr 0 ex :$ V 0)
            :$ (incrementExpr 0 ey :$ V 0)
        -- #testBit b @(∀ t) x y = Λ #testBit b @t (x[+/@0] @0) (y[+/@0] @0)
        IE TestBit :$ eb :@ FA t :$ ex :$ ey -> pure . TL
            $ IE TestBit :$ eb :@ t
            :$ (incrementTypeInExpr 0 ex :@ TV 0)
            :$ (incrementTypeInExpr 0 ey :@ TV 0)
        _ -> empty

    llvmPure :: Expr a -> m (Expr a)
    llvmPure = withLabel $ \case
        IE PureIO :@ t -> case toMaybeInternalType t of
            Just (LlvmInt 0) -> pure $ marshallOut () t
            Just (LlvmInt size) -> pure
                $ t :\ IE (PurePrim size) :$ (marshallOut () t :$ V 0)
            Nothing -> empty
        IE (PurePrim _) :$ IE (LlvmOperand op) -> pure . IE $ LlvmOperand op
        _ -> empty

    llvmBind :: Expr a -> m (Expr a)
    llvmBind = withLabel $ \case
        IE BindIO :@ t -> case toMaybeInternalType t of
            Just (LlvmInt size) -> pure $ IOT t :\ TL
                (t :-> IOT (TV 0) :\ IE (BindPrim size) :$ V 1 :@ TV 0
                :$ (IT (LlvmInt size) :\ V 1 :$ (marshallIn () t :$ V 0)))
            Nothing -> empty
        IE (BindPrim 0) :$ IE Unit :@ _ :$ ef -> pure $ ef :$ IE Unit
        IE (BindPrim _) :$ IE (LlvmOperand op) :@ _ :$ ef -> pure
            $ ef :$ IE (LlvmOperand op)
        _ -> empty

    llvmBits :: Expr a -> m (Expr a)
    llvmBits = withLabel $ \case
        IE (IsolateBit bit size) :$ IE (LlvmOperand op) -> pure . IE $ Emit
            $ IE . LlvmOperand <$> do
                shifted <- emitInstr (LLVM.IntegerType size)
                    $ LLVM.LShr False op (LLVM.ConstantOperand
                        $ LLVM.Constant.Int size $ toInteger bit - 1) []
                emitInstr (LLVM.IntegerType size)
                    $ LLVM.Trunc shifted (LLVM.IntegerType 1) []
        IE TestBit :$ IE (LlvmOperand opc) :@ IT (LlvmInt _)
            :$ IE (LlvmOperand opt) :$ IE (LlvmOperand opf) -> pure . IE $ Emit
                $ IE . LlvmOperand <$> emitInstr (LLVM.IntegerType 1)
                    (LLVM.Select opc opt opf [])
        IE TestBit :$ IE (LlvmOperand opc) :@ IOT t :$ et :$ ef -> do
            pure . IE $ Emit $ IE <$> do
                bt <- fresh
                bf <- fresh
                br <- fresh
                emitTerm $ CondBr opc bt bf []
                emitBlockStart bt
                mopt <- mapExprRewriter (const ()) (normExpr et)
                    >>= emitExprBlock
                emitTerm $ Br br []
                emitBlockStart bf
                mopf <- mapExprRewriter (const ()) (normExpr ef)
                    >>= emitExprBlock
                emitTerm $ Br br []
                emitBlockStart br
                let lt = internalTypeToLlvm $ toInternalType t
                case (,) <$> mopt <*> mopf of
                    Nothing -> pure Unit
                    Just (opt, opf) -> fmap LlvmOperand . emitInstr lt
                        $ LLVM.Phi lt [(opt, bt), (opf, bf)] []
        _ -> empty

    llvmBitVector :: Expr () -> m (Expr ())
    llvmBitVector (IE (BitVector es))
        | all isOperand es = pure . IE $ Emit $ IE . LlvmOperand <$> do
            let size = fromIntegral $ length es :: Word32
                mergeInts x opy = do
                    y <- opy
                    emitInstr (LLVM.IntegerType size)
                        $ LLVM.Or x y []
                xs = toOperand <$> es
            exts <- for xs $ \x -> emitInstr (LLVM.IntegerType 1)
                $ LLVM.ZExt x (LLVM.IntegerType size) []
            sh <- for (zip [0..] exts) $ \(bit, ext)
                -> emitInstr (LLVM.IntegerType size)
                    $ LLVM.Shl False True ext
                        (LLVM.ConstantOperand $ LLVM.Constant.Int size bit) []
            foldr mergeInts (pure . LLVM.ConstantOperand
                $ LLVM.Constant.Int size 0) sh
        | any isEmit es = pure . IE $ Emit
            $ IE . BitVector <$> traverse runEmit es
        | otherwise = empty
    llvmBitVector _ = empty

    llvmCall :: [LLVM.Operand] -> Expr () -> m (Expr ())
    llvmCall ops = \case
        ex :$ IE (LlvmOperand op) -> llvmCall (ops <> pure op) ex
        IE (Call fop argc tret) -> if argc == length ops
            then pure . IE $ Emit $ IE <$> case tret of
                LLVM.VoidType -> Unit <$ emitInstrVoid (callInstr fop ops)
                _ -> LlvmOperand <$> emitInstr tret (callInstr fop ops)
            else empty
        _ -> empty
      where
        callInstr :: Operand -> [LLVM.Operand] -> LLVM.Instruction
        callInstr fop argOps = LLVM.Call Nothing LLVM.CallConv.C [] (Right fop)
            ((, []) <$> argOps) [] []
    
    llvmPointer :: Expr a -> m (Expr a)
    llvmPointer = withLabel $ \case
        IE LoadPointer :@ t :$ IE (LlvmOperand op)
            -> pure . IE $ Emit $ IE . LlvmOperand <$> emitInstr (toPointerType t)
                (LLVM.Load True op atomic 1 [])
        IE StorePointer :@ t -> case toMaybeInternalType t of
            Just it -> pure $ PtrT WritePointer t :\ t
                :\ IE (StorePrim $ internalTypeToLlvm it)
                :$ V 1 :$ (marshallOut () t :$ V 0)
            Nothing -> empty
        IE (StorePrim _) :$ IE (LlvmOperand ptr) :$ IE (LlvmOperand op)
            -> pure . IE $ Emit $ do
                emitInstrVoid $ LLVM.Store True ptr op atomic 1 []
                pure $ IE Unit
        _ -> empty
      where
        atomic :: Maybe LLVM.Atomicity
        atomic = Nothing

    liftEmit :: Expr a -> m (Expr a)
    liftEmit = withLabel $ \case
        IE (Emit ef) :$ ex -> pure . IE $ Emit $ (:$ ex) <$> ef
        ef :$ IE (Emit ex) -> pure . IE $ Emit $ (ef :$) <$> ex
        IE (Emit ef) :@ tx -> pure . IE $ Emit $ (:@ tx) <$> ef
        _ -> empty

    isEmit :: Expr () -> Bool
    isEmit ex = case ex of
        IE (Emit _) -> True
        _ -> False

    runEmit
        :: forall si' n. (Has IRBuilder si' n, Has (Rewriter (Expr ())) si' n)
        => Expr () -> n (Expr ())
    runEmit ex = case ex of
        IE (Emit ey) -> ey
        _ -> pure ex

    isOperand :: Expr () -> Bool
    isOperand ex = case ex of
        IE (LlvmOperand _) -> True
        _ -> False

    toOperand :: Expr () -> LLVM.Operand
    toOperand ex = case ex of
        IE (LlvmOperand op) -> op
        _ -> error $ "rewriteLlvm: expected an LLVM operand, got: "
            <> show (prettyExpr0 ex)

-- | Creates a function that marshalls from an LLVM type to an Elemental type.
marshallIn :: a -> Type a -> Expr a
marshallIn l t = case toInternalType t of
    LlvmInt 0 -> Lam l (SpecialType l . InternalType l $ LlvmInt 0) . TypeLam l
        . Lam l (TypeVar l 0) $ Var l 0
    LlvmInt 1 -> InternalExpr l TestBit
    LlvmInt size -> Lam l (SpecialType l . InternalType l $ LlvmInt size)
        . TypeLam l . Lam l (foldr (const . Arrow l $ BitType l)
            (TypeVar l 0) [0 .. size - 1])
        . foldr (flip $ App l) (Var l 0)
        $ App l (InternalExpr l TestBit) . flip (App l) (Var l 1)
            . InternalExpr l . flip IsolateBit size <$> [1 .. size]

-- | Creates a function that marshalls from an Elemental type to an LLVM type.
marshallOut :: a -> Type a -> Expr a
marshallOut l t = case toInternalType t of
    LlvmInt 0 -> Lam l t $ InternalExpr l Unit
    LlvmInt 1 -> Lam l t . marshallBit l $ Var l 0
    LlvmInt size -> Lam l t . App l (TypeApp l (Var l 0)
            (SpecialType l . InternalType l $ LlvmInt size))
        . foldr (Lam l) (InternalExpr l . BitVector
            $ marshallBit l . Var l <$> [0 .. fromIntegral size - 1])
        . replicate (fromIntegral size) $ BitType l
  where
    marshallBit :: a -> Expr a -> Expr a
    marshallBit l' ea = App l' (App l' (TypeApp l' ea . SpecialType l'
            . InternalType l' $ LlvmInt 1)
        . InternalExpr l' . LlvmOperand . LLVM.ConstantOperand
            $ LLVM.Constant.Int 1 1)
        . InternalExpr l' . LlvmOperand . LLVM.ConstantOperand
            $ LLVM.Constant.Int 1 0

{-|
    Splits a foreign function type into its argument types and its return type,
    failing if the return type isn't in @IO@.
-}
splitArrow :: Type a -> ([Type a], Type a)
splitArrow ta = fromMaybe (error $ "splitArrow: expected IO return type, got: "
    <> show (prettyType0 ta)) $ maybeSplitArrow ta

{-|
    Splits an Elemental arrow type and converts the argument and return types to
    LLVM types.

    See also 'splitArrow'.
-}
arrowToLlvmType :: Type a -> ([LLVM.Type], LLVM.Type)
arrowToLlvmType = splitArrow >>> fmap toArgumentType *** toReturnType

{-|
    Converts an Elemental type to an LLVM type, failing if the type is not a
    pointer type or if it is unmarshallable.
-}
toPointerType :: Type a -> LLVM.Type
toPointerType t = fromMaybe abort $ toMaybePointerType t
  where
    abort :: a
    abort = error $ "toPointerType: unmarshallable pointer type: "
        <> show (prettyType0 t)

{-|
    Converts an Elemental type to an LLVM type, failing if the type is @void@ or
    unmarshallable.
-}
toArgumentType :: Type a -> LLVM.Type
toArgumentType t = fromMaybe abort $ toMaybeArgumentType t
  where
    abort :: a
    abort = error $ "toArgumentType: unmarshallable argument type: "
        <> show (prettyType0 t)

-- | Converts an Elemental type to an LLVM return type.
toReturnType :: Type a -> LLVM.Type
toReturnType t = case toInternalType t of
    LlvmInt 0 -> LLVM.VoidType
    LlvmInt size -> LLVM.IntegerType size

-- | Converts a type to an internal type, failing if the conversion is invalid.
toInternalType :: Type a -> InternalType a
toInternalType t = fromMaybe (error $ "toInternalType: unmarshallable type: "
    <> show (prettyType0 t)) $ toMaybeInternalType t

-- | Converts an internal Elemental type to an LLVM type.
internalTypeToLlvm :: InternalType a -> LLVM.Type
internalTypeToLlvm it = case it of
    LlvmInt size -> LLVM.IntegerType size
