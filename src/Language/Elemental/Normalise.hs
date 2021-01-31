{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Functions for normalising programs.
module Language.Elemental.Normalise
    ( normaliseProgram
    , normaliseExpr
    , rewriteExpr
    , inlineRefs
    , substituteExpr
    , substituteTypeInExpr
    , substituteType
    , incrementExpr
    , incrementTypeInExpr
    , incrementType
    ) where

import Control.Carrier.State.Church (StateC(StateC), get, modify, runState)
import Control.Effect.Choose (Choose, (<|>))
import Control.Effect.Empty (Empty, Has, empty)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)

import Language.Elemental.Rewrite
import Language.Elemental.Syntax.Internal


-- | Replaces all defined references and normalises all expressions.
normaliseProgram
    :: forall a sig m. Has (Rewriter (Expr a)) sig m
    => Program a -> m (Program a)
normaliseProgram (Program l decls) = runState (const pure) defaults
    $ Program l . filter isForeign <$> traverse normaliseDecl decls
  where
    defaults :: M.Map Name (Expr a)
    defaults = M.empty

    normaliseDecl :: Decl a -> StateC (M.Map Name (Expr a)) m (Decl a)
    normaliseDecl decl = case decl of
        Binding l' dname@(DeclName _ name) expr -> do
            scope <- get
            normExpr <- lift . normaliseExpr $ inlineRefs scope expr
            modify $ M.insert name normExpr
            pure $ Binding l' dname normExpr
        ForeignImport {} -> pure decl
        ForeignExport l' foreignName expr t -> do
            scope <- get
            normExpr <- lift . normaliseExpr $ inlineRefs scope expr
            pure $ ForeignExport l' foreignName normExpr t
        ForeignPrimitive {} -> pure decl

    isForeign :: Decl a -> Bool
    isForeign decl = case decl of
        Binding {} -> False
        ForeignImport {} -> True
        ForeignExport {} -> True
        ForeignPrimitive {} -> True

    lift :: m r -> StateC s m r
    lift m = StateC $ \k s -> m >>= k s

-- | Normalises an expression.
normaliseExpr :: Has (Rewriter (Expr a)) sig m => Expr a -> m (Expr a)
normaliseExpr = beginRewrite $ rewriteExpr $ const empty

-- | Attempts to perform a single rewrite step.
rewriteExpr
    :: forall a sig m. (Has Choose sig m, Has Empty sig m)
    => (Expr a -> m (Expr a))
    -- ^ Extra rules to try before rewriting deeper but after all other rules.
    -> Expr a -> m (Expr a)
rewriteExpr rewriteExtra = rewrite
  where
    rewrite :: Expr a -> m (Expr a)
    rewrite ex = rewritePure ex <|> rewriteExtra ex <|> rewriteInner ex

    {-
        These rules must run last to avoid over-aggressively lifting 'Emit' out,
        as that interferes with the LLVM code generation. By running other
        rewrite rules first, other rules could eliminate the 'Emit' expression
        completely. For example, if a program discards an @IO@ value, then the
        last thing we want is to still emit its LLVM.
    -}
    rewriteInner :: Expr a -> m (Expr a)
    rewriteInner ea = case ea of
        Ref {} -> empty
        Var {} -> empty
        App l ef ex -> flip (App l) ex <$> rewrite ef
            <|> App l ef <$> rewrite ex
        TypeApp l ef t -> flip (TypeApp l) t <$> rewrite ef
        Lam l t ex -> Lam l t <$> rewrite ex
        TypeLam l ex -> TypeLam l <$> rewrite ex
        InternalExpr l iex -> InternalExpr l <$> case iex of
            Unit {} -> empty
            PureIO {} -> empty
            BindIO {} -> empty
            PurePrim {} -> empty
            BindPrim {} -> empty
            BitVector es -> BitVector <$> rewriteAny rewrite es
            Call {} -> empty
            IsolateBit {} -> empty
            TestBit -> empty
            LlvmOperand {} -> empty
            {-
                We can't conditionally rewrite Emit due to the monad, so we
                don't normalise it here.
            -}
            Emit {} -> empty

    rewritePure :: Expr a -> m (Expr a)
    rewritePure ea = case ea of
        (_ :\ ex) :$ ey -> pure $ substituteExpr 0 ey ex
        TypeLam _ ex :@ t -> pure $ substituteTypeInExpr 0 t ex
        _ -> empty

{-|
    Inlines references following the given mapping. If a referenced name cannot
    be found in the mapping, then it will be left untouched.
    This assumes that the inlined expressions do not contain any free variables.
-}
inlineRefs :: M.Map Name (Expr a) -> Expr a -> Expr a
inlineRefs scope expr = case expr of
    Ref _ ref -> fromMaybe expr $ scope M.!? ref
    Var {} -> expr
    App l ef ex -> App l (inlineRefs scope ef) (inlineRefs scope ex)
    TypeApp l ef t -> TypeApp l (inlineRefs scope ef) t
    Lam l t ex -> Lam l t (inlineRefs scope ex)
    TypeLam l ex -> TypeLam l (inlineRefs scope ex)
    InternalExpr {} -> expr

{-|
    Substitutes the first given expression with the given de Bruijn index into
    the second given expression.
-}
substituteExpr :: Int -> Expr a -> Expr a -> Expr a
substituteExpr idx ea eb = case eb of
    Ref {} -> eb
    Var l idx' -> case compare idx' idx of
        LT -> Var l idx'
        EQ -> ea
        GT -> Var l (idx' - 1)
    App l ef ex -> App l (substituteExpr idx ea ef) (substituteExpr idx ea ex)
    TypeApp l ef t -> TypeApp l (substituteExpr idx ea ef) t
    Lam l t ex -> Lam l t (substituteExpr (idx + 1) (incrementExpr 0 ea) ex)
    TypeLam l ex -> TypeLam l (substituteExpr idx ea ex)
    InternalExpr l iex -> InternalExpr l $ case iex of
        Unit {} -> iex
        PureIO {} -> iex
        BindIO {} -> iex
        PurePrim {} -> iex
        BindPrim {} -> iex
        BitVector es -> BitVector $ substituteExpr idx ea <$> es
        Call {} -> iex
        IsolateBit {} -> iex
        TestBit -> iex
        LlvmOperand {} -> iex
        Emit mex -> Emit $ substituteExpr idx ea <$> mex

{-|
    Substitutes the given type with the given de Bruijn index into the given
    expression.
-}
substituteTypeInExpr :: Int -> Type a -> Expr a -> Expr a
substituteTypeInExpr idx ta eb = case eb of
    Ref {} -> eb
    Var {} -> eb
    App l ef ex -> App l (substituteTypeInExpr idx ta ef)
        (substituteTypeInExpr idx ta ex)
    TypeApp l ef t
        -> TypeApp l (substituteTypeInExpr idx ta ef) (substituteType idx ta t)
    Lam l t ex
        -> Lam l (substituteType idx ta t) (substituteTypeInExpr idx ta ex)
    TypeLam l ex
        -> TypeLam l (substituteTypeInExpr (idx + 1) (incrementType 0 ta) ex)
    InternalExpr l iex -> InternalExpr l $ case iex of
        Unit {} -> iex
        PureIO {} -> iex
        BindIO {} -> iex
        PurePrim {} -> iex
        BindPrim {} -> iex
        BitVector es -> BitVector $ substituteTypeInExpr idx ta <$> es
        Call {} -> iex
        IsolateBit {} -> iex
        TestBit -> iex
        LlvmOperand {} -> iex
        Emit mex -> Emit $ substituteTypeInExpr idx ta <$> mex

{-|
    Substitutes the first given type with the given de Bruijn index into the
    second given type.
-}
substituteType :: Int -> Type a -> Type a -> Type a
substituteType idx ta tb = case tb of
    Arrow l tx ty
        -> Arrow l (substituteType idx ta tx) (substituteType idx ta ty)
    Forall l tx -> Forall l (substituteType (idx + 1) (incrementType 0 ta) tx)
    TypeVar l idx' -> case compare idx' idx of
        LT -> TypeVar l idx'
        EQ -> ta
        GT -> TypeVar l (idx' - 1)
    SpecialType l st -> SpecialType l $ case st of
        IOType l' tx -> IOType l' (substituteType idx ta tx)
        InternalType {} -> st

{-|
    Increments every 'Var' de Bruijn index greater than or equal to the given
    index.
-}
incrementExpr :: Int -> Expr a -> Expr a
incrementExpr idx ea = case ea of
    Ref {} -> ea
    Var l idx' -> Var l (if idx' >= idx then idx' + 1 else idx')
    App l ef ex -> App l (incrementExpr idx ef) (incrementExpr idx ex)
    TypeApp l ef t -> TypeApp l (incrementExpr idx ef) t
    Lam l t ex -> Lam l t (incrementExpr (idx + 1) ex)
    TypeLam l ex -> TypeLam l (incrementExpr idx ex)
    InternalExpr l iexpr -> InternalExpr l $ case iexpr of
        Unit {} -> iexpr
        PureIO {} -> iexpr
        BindIO {} -> iexpr
        PurePrim {} -> iexpr
        BindPrim {} -> iexpr
        BitVector es -> BitVector $ incrementExpr idx <$> es
        Call {} -> iexpr
        IsolateBit {} -> iexpr
        TestBit -> iexpr
        LlvmOperand {} -> iexpr
        Emit mex -> Emit $ incrementExpr idx <$> mex

{-|
    Increments every 'TypeVar' de Bruijn index greater than or equal to the
    given index.
-}
incrementTypeInExpr :: Int -> Expr a -> Expr a
incrementTypeInExpr idx ea = case ea of
    Ref {} -> ea
    Var {} -> ea
    App l ef ex
        -> App l (incrementTypeInExpr idx ef) (incrementTypeInExpr idx ex)
    TypeApp l ef t
        -> TypeApp l (incrementTypeInExpr idx ef) (incrementType idx t)
    Lam l t ex -> Lam l (incrementType idx t) (incrementTypeInExpr idx ex)
    TypeLam l ex -> TypeLam l (incrementTypeInExpr (idx + 1) ex)
    InternalExpr l iexpr -> InternalExpr l $ case iexpr of
        Unit {} -> iexpr
        PureIO {} -> iexpr
        BindIO {} -> iexpr
        PurePrim {} -> iexpr
        BindPrim {} -> iexpr
        BitVector es -> BitVector $ incrementTypeInExpr idx <$> es
        Call {} -> iexpr
        IsolateBit {} -> iexpr
        TestBit -> iexpr
        LlvmOperand {} -> iexpr
        Emit mex -> Emit $ incrementTypeInExpr idx <$> mex

{-|
    Increments every 'TypeVar' de Bruijn index greater than or equal to the
    given index.
-}
incrementType :: Int -> Type a -> Type a
incrementType idx ta = case ta of
    Arrow l tx ty -> Arrow l (incrementType idx tx) (incrementType idx ty)
    Forall l tx -> Forall l (incrementType (idx + 1) tx)
    TypeVar l idx' -> TypeVar l (if idx' >= idx then idx' + 1 else idx')
    SpecialType l st -> SpecialType l $ case st of
        IOType l' tx -> IOType l' (incrementType idx tx)
        InternalType {} -> st
