{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Functions to convert an Elemental program into an LLVM module.
module Language.Elemental.Emit
    ( emitProgram
    ) where

import Control.Applicative (empty)
import Control.Monad (void, (>=>))
import Data.Fix (Fix(Fix), unFix)
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text.Short (toShortByteString)
import Data.Traversable (for)
import Data.Word (Word32)
import LLVM.AST (Definition, Operand, Terminator(Br, CondBr, Ret))
import LLVM.AST qualified as LLVM
import LLVM.AST.CallingConvention qualified as LLVM.CallConv
import LLVM.AST.Constant qualified as LLVM.Constant
import Prettyprinter (Doc, PageWidth(Unbounded), defaultLayoutOptions, group, layoutPageWidth, layoutPretty)
import Prettyprinter.Render.String (renderString)

import Control.Carrier.ModuleBuilder
import Control.Effect.IRBuilder
import Language.Elemental.Emit.Internal
import Language.Elemental.Normalise
import Language.Elemental.Primitive
import Language.Elemental.Rewrite
import Language.Elemental.Syntax.Internal
import Language.Elemental.Syntax.Pretty
import Language.Elemental.Syntax.Synonyms


-- | Converts an Elemental program into a list of LLVM definitions.
emitProgram
    :: (Has (Birewriter ExprF TypeF) sig m) => Program a -> m [Definition]
emitProgram p = runModuleBuilder (const . pure) emptyModuleBuilder
    $ inlinePrimitives p >>= normaliseProgram (normRules <> emitRules)
    >>= traverse emitDecl . getDecls
  where
    getDecls :: Program a -> [Decl a]
    getDecls (Program _ decls) = decls

emitDecl
    :: (Has (Birewriter ExprF TypeF) sig m, Has ModuleBuilder sig m)
    => Decl a -> m ()
emitDecl = \case
    Binding {} -> pure ()
    ForeignImport {} -> error "emitDecl: unexpected foreign import"
    ForeignExport _ foreignName expr t -> do
        let (targs, tret) = splitArrow $ stripType t
            ltargs = toArgumentType <$> targs
            ltret = toReturnType tret
        void . function (LLVM.Name $ toShortByteString foreignName) ltargs ltret
            $ applyArgs (stripExpr expr) targs tret >=> emitExpr
    ForeignPrimitive {} -> error "emitDecl: unexpected foreign primitive"
    ForeignAddress {} -> error "emitDecl: unexpected foreign address"
  where
    applyArgs
        :: Has (Birewriter ExprF TypeF) sig m
        => Expr -> [Type] -> Type -> [Operand] -> m Expr
    applyArgs expr targs tret ops = birewriteM (normRules <> emitRules) $ foldr
        (\(t, op) eb -> marshallIn t tret
            :$: IE (LlvmValue $ LlvmOperand op) :$: (t :\: eb))
        (foldr (flip (:$:) . V) expr . reverse $ zipWith const [0..] targs)
        . reverse $ zip targs ops

-- | Emits an expression with a return instruction and starts a new basic block.
emitExpr :: (Has (Birewriter ExprF TypeF) sig m, Has IRBuilder sig m) => Expr -> m ()
emitExpr expr = do
    lv <- emitExprBlock expr
    emitTerm $ Ret (toMOp lv) []
    void block
  where
    toMOp :: LlvmValue -> Maybe LLVM.Operand
    toMOp LlvmUnit = Nothing
    toMOp (LlvmOperand op) = pure op

-- | Emits a basic block for an expression.
emitExprBlock
    :: forall sig m. (Has (Birewriter ExprF TypeF) sig m, Has IRBuilder sig m)
    => Expr -> m LlvmValue
emitExprBlock = \case
    IE (LlvmIO (LlvmGen mlv)) -> mlv
    IE (BindPrim _ (IE TestBit :$: IE (LlvmValue (LlvmOperand opc))) t ey)
        -> do
            bt <- fresh
            bf <- fresh
            br <- fresh
            emitTerm $ CondBr opc bt bf []
            emitBlockStart bt
            lvt <- bindTo ey . TL $ TV 0 :\: TV 0 :\: V 1
            bt' <- currentBlock
            emitTerm $ Br br []
            emitBlockStart bf
            lvf <- bindTo ey . TL $ TV 0 :\: TV 0 :\: V 0
            bf' <- currentBlock
            emitTerm $ Br br []
            emitBlockStart br
            let lt = internalTypeToLlvm $ toInternalType t
            case (lvt, lvf) of
                (LlvmOperand opt, LlvmOperand opf) -> fmap LlvmOperand
                    . emitInstr lt $ LLVM.Phi lt [(opt, bt'), (opf, bf')] []
                (_, _) -> pure LlvmUnit
    IE (BindPrim _ ex _ ey) -> do
        lv <- emitExprBlock ex
        bindTo ey $ IE $ LlvmValue lv
    expr -> error $ "Emit.emitExprBlock: illegal expression: "
        <> showDoc (prettyExpr0 expr)
  where
    bindTo :: Expr -> Expr -> m LlvmValue
    bindTo e e'
        = birewriteM (normRules <> emitRules) (e :$: e') >>= emitExprBlock

-- | Converts primitives and foreign imports into the corresponding bindings.
inlinePrimitives :: Has ModuleBuilder sig m => Program a -> m (Program a)
inlinePrimitives (Program l decls) = Program l <$> traverse go decls
  where
    go :: Has ModuleBuilder sig m => Decl a -> m (Decl a)
    go decl = case decl of
        Binding {} -> pure decl
        ForeignImport l' name foreignName t -> Binding l' name <$> do
            let (targs, tret) = splitArrow $ stripType t
                ltargs = toArgumentType <$> targs
                ltret = toReturnType tret
                argc = length targs
            func <- extern (LLVM.Name $ toShortByteString foreignName)
                ltargs ltret
            pure . annExpr l' $ foldr (:\:) (foldr
                (\targ eb -> marshallOut targ tret :$: V (argc - 1)
                    :$: (IT (toInternalType targ) :\: eb))
                (foldr (flip (:$:)) (IE $ Call func argc ltret)
                    $ V <$> zipWith const [0..] targs)
                targs) targs
        ForeignExport {} -> pure decl
        ForeignPrimitive l' dname@(DeclName _ name) _ -> pure . Binding l' dname
            . annExpr l' . snd $ primitives M.! name
        ForeignAddress l' dname addr t -> pure . Binding l' dname
            . Fix . Biann l' . InternalExpr
            . LlvmValue . LlvmOperand . LLVM.ConstantOperand
            {-
                With GHC 9, it would be sounder to work out the needed bit size
                using the base 2 logarithm. Although it can be done with older
                GHC versions, differences between the two integer libraries make
                it necessary to test on both libraries.

                Until GHC 9, 128 bits should work for most architectures. Using
                a smaller size than the architecture may truncate the address.
            -}
            . LLVM.Constant.IntToPtr (LLVM.Constant.Int 128 addr)
            $ toPointerType $ stripType t

-- | Rewrite rules for emitting Elemental expressions as LLVM.
emitRules :: [Birule ExprF TypeF]
emitRules =
    -- bindIO @t (pureIO @t x) = (λt Λ λ(t → IO 0) 0 1) x
    [ IEI BindIO :@? st 0 :$? (IEI PureIO :@? st 0 :$? se 0)
        :=> (gt 0 :\= TLO (gt 0 :->= IOTO (TVO 0) :\= VO 0 :$= VO 1)) :$= ge 0
    -- Reassociate binds
    , Fix (Bisome $ \case
        IE (BindPrim size1 (IE (BindPrim size2 ex t1 ey)) t2 ez) -> pure
            ( IM.fromList [(0, size1), (1, size2)]
            , IM.fromList [(0, ex), (1, ey), (2, ez)]
            , IM.fromList [(0, t1), (1, t2)]
            )
        _ -> mempty
        ) :=> Fix (Bidynamic $ \(sizes, es, ts) -> IE
            . BindPrim (sizes ! 1) (es ! 0) (ts ! 1)
            $ IT (LlvmInt $ sizes ! 0) :\: IE (BindPrim
                (sizes ! 0) (incrementExpr 0 (es ! 1) :$: V 0)
                (ts ! 1) (incrementExpr 0 $ es ! 2)
                )
            )
    , IEI BindIO :@? st 0 :$? Fix (Bisome $ \case
        IE (BindPrim size ex t ey) -> pure (IM.singleton 0 t,
            (IM.singleton 0 size, IM.fromList [(0, ex), (1, ey)]))
        _ -> mempty
        ) :@? st 1 :$? pe (pure . mkSnd . mkSnd) 2
        :=> Fix (Bidynamic $ \(ts, (sizes, es)) -> IE
            . BindPrim (sizes ! 0) (es ! 0) (ts ! 1)
            $ IT (LlvmInt $ sizes ! 0) :\: IE BindIO
                :@: ts ! 0 :$: (incrementExpr 0 (es ! 1) :$: V 0)
                :@: ts ! 1 :$: incrementExpr 0 (es ! 2)
            )
    -- pureIO rules
    , IEI PureIO :@? Fix (Some $ \t -> (,) (IM.singleton 0 t) . IM.singleton 0
        <$> toMaybeInternalType t)
        :=> gt 0 :\= Fix
            (Bidynamic $ \(ts, its) -> marshallOut (ts ! 0) (IT $ its ! 0)
                :$: V 0 :$: case its ! 0 of
                    LlvmInt size -> IE $ PurePrim size
                    )
    , Fix (Bisome $ \case
        Fix (InternalExpr (PurePrim _)) -> pure mempty
        _ -> mempty
        ) :$? Fix (Bisome $ \case
            Fix (InternalExpr (LlvmValue lv)) -> pure $ IM.singleton 0 lv
            _ -> mempty
            )
        :=> re id 0 (Fix . InternalExpr . pureLlvmIO)
    -- Convert bindIO @t to #bindi{n} when t is marshallable
    , IEI BindIO :@? Fix (Some
            $ \t -> mkFst . (,) (IM.singleton 1 t) . IM.singleton 0
                <$> toMaybeInternalType t)
        :$? se 0 :@? pt (pure . mkFst . mkFst) 0 :$? se 1
        :=> Fix (Bidynamic $ \((ts, its), es) -> let LlvmInt size = its ! 0
            in IE $ BindPrim size (es ! 0) (ts ! 0) (IT (its ! 0)
                :\: marshallIn (ts ! 1) (ts ! 0) :$: V 0
                    :$: incrementExpr 0 (es ! 1))
                )
    -- Bitwise arithmetic
    , Fix (Bisome $ \v -> case unFix v of
        InternalExpr (IsolateBit bit size)
            -> pure . mkSnd $ IM.singleton 0 (bit, size)
        _ -> mempty
        ) :$? pop mkFst 0
        :=> Fix (Bidynamic $ \(ops, bits) -> Fix . InternalExpr $ LlvmIO
            $ LlvmGen $ do
                let op = ops ! 0
                    (bit, size) = bits ! 0
                shifted <- emitInstr (LLVM.IntegerType size)
                    $ LLVM.LShr False op (LLVM.ConstantOperand
                        $ LLVM.Constant.Int size $ toInteger bit - 1) []
                fmap LlvmOperand . emitInstr (LLVM.IntegerType size)
                    $ LLVM.Trunc shifted (LLVM.IntegerType 1) []
            )
    -- Bit vector
    , Fix (Bisome $ \case
        IE (BitVector es) -> IM.singleton 0 <$> traverse mop es
        _ -> mempty)
        :=> Fix (Bidynamic $ \opss -> IE $ LlvmIO $ LlvmGen $ do
            let ops = opss ! 0
                mergeInts x opy = do
                    y <- opy
                    emitInstr (LLVM.IntegerType size) $ LLVM.Or x y []
                size = fromIntegral $ length ops :: Word32
            exts <- for ops $ \x -> emitInstr (LLVM.IntegerType 1)
                $ LLVM.ZExt x (LLVM.IntegerType size) []
            sh <- for (zip [0..] exts) $ \(bit, ext)
                -> emitInstr (LLVM.IntegerType size) $ LLVM.Shl False True ext
                    (LLVM.ConstantOperand $ LLVM.Constant.Int size bit) []
            LlvmOperand <$> foldr mergeInts
                (pure . LLVM.ConstantOperand $ LLVM.Constant.Int size 0) sh
            )
    -- Call
    , Fix (Bisome $ (IM.singleton 0 <$>) . matchCall [])
        :=> Fix (Bidynamic $ \ios -> IE $ LlvmIO $ ios ! 0)
    -- Pointer rules
    , IEI LoadPointer :@? st 0 :$? pop mkSnd 0
        :=> Fix (Bidynamic $ \(ts, ops) -> IE . LlvmIO $ LlvmGen
            $ LlvmOperand <$> emitInstr (toPointerType $ ts ! 0)
                (LLVM.Load True (ops ! 0) atomic 1 []))
    , IEI StorePointer :@? Fix (Some $ \t -> (,) (IM.singleton 0 t)
            . IM.singleton 0 <$> toMaybeInternalType t)
        :=> ft 0 (PtrT WritePointer) :\= gt 0 :\= Fix (Bidynamic
            $ \(ts, its) -> marshallOut (ts ! 0) (FA $ TV 0 :->: TV 0)
                :$: V 0 :$: (IT (its ! 0)
                    :\: IE (StorePrim . internalTypeToLlvm $ its ! 0)
                    :$: V 2 :$: V 0))
    , Fix (Bisome $ \case
        IE (StorePrim _) -> pure mempty
        _ -> mempty
        ) :$? pop id 0 :$? pop id 1
        :=> Fix (Bidynamic $ \ops -> IE . LlvmIO $ LlvmGen $ do
            emitInstrVoid $ LLVM.Store True (ops ! 0) (ops ! 1) atomic 1 []
            pure LlvmUnit
            )
    ]
  where
    st :: Monoid b => Int -> TypeRuleIn (IM.IntMap Type, b)
    st = pt $ pure . mkFst

    se :: Monoid b => Int -> ExprRuleIn (b, IM.IntMap Expr)
    se = pe $ pure . mkSnd

    pt :: (IM.IntMap Type -> Maybe a) -> Int -> TypeRuleIn a
    pt f idx = Fix . Some $ f . IM.singleton idx

    pe :: (IM.IntMap Expr -> Maybe a) -> Int -> ExprRuleIn a
    pe f idx = Fix . Bisome $ f . IM.singleton idx

    gt :: Int -> TypeRuleOut (IM.IntMap Type, b)
    gt idx = ft idx id

    ge :: Int -> ExprRuleOut (b, IM.IntMap Expr)
    ge idx = fe idx id

    ft :: Int -> (a -> Type) -> TypeRuleOut (IM.IntMap a, b)
    ft = rt fst

    fe :: Int -> (a -> Expr) -> ExprRuleOut (b, IM.IntMap a)
    fe = re snd

    rt :: (a -> IM.IntMap a') -> Int -> (a' -> Type) -> TypeRuleOut a
    rt sel idx f = Fix . Dynamic $ \s -> f $ sel s ! idx

    re :: (a -> IM.IntMap a') -> Int -> (a' -> Expr) -> ExprRuleOut a
    re sel idx f = Fix . Bidynamic $ \s -> f $ sel s ! idx

    mkFst :: Monoid b => a -> (a, b)
    mkFst x = (x, mempty)

    mkSnd :: Monoid b => a -> (b, a)
    mkSnd x = (mempty, x)

    pop :: (IM.IntMap LLVM.Operand -> a) -> Int -> ExprRuleIn a
    pop f idx = Fix . Bisome $ (f . IM.singleton idx <$>) . mop
    
    mop :: Expr -> Maybe LLVM.Operand
    mop (IE (LlvmValue (LlvmOperand op))) = pure op
    mop _ = empty
    
    pureLlvmIO :: LlvmValue -> InternalExpr t rec
    pureLlvmIO v = LlvmIO $ LlvmGen $ pure v

    matchCall :: [LLVM.Operand] -> Expr -> Maybe (LlvmGen LlvmValue)
    matchCall ops = \case
        ex :$: IE (LlvmValue (LlvmOperand op)) -> matchCall (op : ops) ex
        IE (Call fop argc tret) -> if argc == length ops
            then pure $ LlvmGen $ case tret of
                LLVM.VoidType -> LlvmUnit <$ emitInstrVoid (callInstr fop ops)
                _ -> LlvmOperand <$> emitInstr tret (callInstr fop ops)
            else empty
        _ -> empty
      where
        callInstr :: Operand -> [LLVM.Operand] -> LLVM.Instruction
        callInstr fop argOps = LLVM.Call Nothing LLVM.CallConv.C [] (Right fop)
            ((, []) <$> argOps) [] []
    
    atomic :: Maybe LLVM.Atomicity
    atomic = Nothing

-- | Creates a function that marshalls from an LLVM type to an Elemental type.
marshallIn :: Type -> Type -> Expr
marshallIn t tret = IT it :\: t :->: IOT tret :\: case it of
    LlvmInt 0 -> V 0 :$: TL (TV 0 :\: V 0)
    LlvmInt 1 -> IE $ BindPrim 1 (IE TestBit :$: V 1) tret (V 0)
    LlvmInt size -> foldr (\idx expr -> IE $ BindPrim 1 (IE $ BindPrim
            1 (IE (IsolateBit idx size) :$: toV idx)
            BitType (IE TestBit)
        ) t $ BitType :\: expr)
        (toV size :$: TL (foldr (const (BitType :->:)) (TV 0) [1..size]
            :\: foldr (flip (:$:)) (V 0) (toV . (size-) <$> [0..size-1])
            ))
        [1..size]
  where
    it :: InternalType Type
    it = toInternalType t

    toV :: Word32 -> Expr
    toV idx = V $ fromIntegral idx

-- | Creates a function that marshalls from an Elemental type to an LLVM type.
marshallOut :: Type -> Type -> Expr
marshallOut t tret = t :\: IT it :->: IOT tret :\: case it of
    LlvmInt 0 -> V 0 :$: IE (LlvmValue LlvmUnit)
    LlvmInt 1 -> V 0 :$: marshallBit (V 1)
    LlvmInt size -> IE $ BindPrim size (V 1 :@: IOT t :$: foldr (:\:)
        (IE . BitVector $ marshallBit . toV <$> [0..size-1])
        (fromIntegral size `replicate` BitType)
        ) tret (V 0)
  where
    it :: InternalType Type
    it = toInternalType t

    marshallBit :: Expr -> Expr
    marshallBit ea = ea :@: IT (LlvmInt 1) :$: IE (LlvmValue $ LlvmOperand
            $ LLVM.ConstantOperand $ LLVM.Constant.Int 1 1)
        :$: IE (LlvmValue $ LlvmOperand
            $ LLVM.ConstantOperand $ LLVM.Constant.Int 1 0)

    toV :: Word32 -> Expr
    toV = V . fromIntegral

{-|
    Splits a foreign function type into its argument types and its return type,
    failing if the return type isn't in @IO@.
-}
splitArrow :: Type -> ([Type], Type)
splitArrow ta = fromMaybe (error $ "splitArrow: expected IO return type, got: "
    <> showDoc (prettyType0 ta)) $ maybeSplitArrow ta

{-|
    Converts an Elemental type to an LLVM type, failing if the type is not a
    pointer type or if it is unmarshallable.
-}
toPointerType :: Type -> LLVM.Type
toPointerType t = fromMaybe abort $ toMaybePointerType t
  where
    abort :: a
    abort = error $ "toPointerType: unmarshallable pointer type: "
        <> showDoc (prettyType0 t)

{-|
    Converts an Elemental type to an LLVM type, failing if the type is @void@ or
    unmarshallable.
-}
toArgumentType :: Type -> LLVM.Type
toArgumentType t = fromMaybe abort $ toMaybeArgumentType t
  where
    abort :: a
    abort = error $ "toArgumentType: unmarshallable argument type: "
        <> showDoc (prettyType0 t)

-- | Converts an Elemental type to an LLVM return type.
toReturnType :: Type -> LLVM.Type
toReturnType t = case toInternalType t of
    LlvmInt 0 -> LLVM.VoidType
    LlvmInt size -> LLVM.IntegerType size

-- | Converts a type to an internal type, failing if the conversion is invalid.
toInternalType :: Type -> InternalType Type
toInternalType t = fromMaybe (error $ "toInternalType: unmarshallable type: "
    <> showDoc (prettyType0 t)) $ toMaybeInternalType t

-- | Converts an internal Elemental type to an LLVM type.
internalTypeToLlvm :: InternalType a -> LLVM.Type
internalTypeToLlvm it = case it of
    LlvmInt size -> LLVM.IntegerType size

-- | Renders a Doc without line breaks
showDoc :: Doc ann -> String
showDoc = renderString . layoutPretty layoutOpts . group
    where
    layoutOpts = defaultLayoutOptions
        { layoutPageWidth = Unbounded
        }
