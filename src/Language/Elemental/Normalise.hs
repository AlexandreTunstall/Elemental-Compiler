{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Functions for normalising programs.
module Language.Elemental.Normalise
    ( normaliseProgram
    , normRules
    , inlineRefs
    , substituteExpr
    , substituteTypeInExpr
    , substituteType
    , incrementExpr
    , incrementTypeInExpr
    , incrementType
    ) where

import Control.Carrier.State.Church (Has, StateC, get, modify, runState)
import Data.Fix (Fix(Fix), foldFix)
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Maybe (fromMaybe)

import Language.Elemental.Rewrite
import Language.Elemental.Syntax.Internal


-- | Replaces all defined references and normalises all expressions.
normaliseProgram
    :: forall a sig m. Has (Birewriter ExprF TypeF) sig m
    => [Birule ExprF TypeF] -> Program a -> m (Program a)
normaliseProgram rules (Program l decls) = runState (const pure) defaults
    $ Program l . filter isForeign <$> traverse normaliseDecl decls
  where
    defaults :: M.Map Name Expr
    defaults = M.empty

    normaliseDecl :: Decl a -> StateC (M.Map Name Expr) m (Decl a)
    normaliseDecl decl = case decl of
        Binding l' dname@(DeclName _ name) expr -> do
            scope <- get
            nex <- birewriteM rules . inlineRefs scope $ stripExpr expr
            modify $ M.insert name nex
            pure $ Binding l' dname $ annExpr l' nex
        ForeignImport {} -> pure decl
        ForeignExport l' foreignName expr t -> do
            scope <- get
            nex <- birewriteM rules . inlineRefs scope $ stripExpr expr
            pure $ ForeignExport l' foreignName (annExpr l' nex) t
        ForeignPrimitive {} -> pure decl
        ForeignAddress {} -> pure decl

    isForeign :: Decl a -> Bool
    isForeign decl = case decl of
        Binding {} -> False
        ForeignImport {} -> True
        ForeignExport {} -> True
        ForeignPrimitive {} -> True
        ForeignAddress {} -> True

-- | Rewrite rules for normalising expressions.
normRules :: [Birule ExprF TypeF]
normRules =
    -- App/Lam β-reduction
    [ Fix (Bimatch $ App
        (Fix . Bimatch . Lam (Fix . Some . const $ pure mempty)
            . Fix . Bisome $ pure . IM.singleton 0)
        (Fix $ Bisome $ pure . IM.singleton 1)
        ) :=> Fix (Bidynamic $ \s -> substituteExpr 0 (s ! 1) (s ! 0))
    -- TypeApp/TypeLam β-reduction
    , Fix (Bimatch $ TypeApp
        (Fix . Bimatch . TypeLam . Fix . Bisome $ pure . pt . IM.singleton 0)
        (Fix . Some $ pure . pe . IM.singleton 0)
        ) :=> Fix
            (Bidynamic $ \s -> substituteTypeInExpr 0 (snd s ! 0) (fst s ! 0))
    ]
  where
    pt :: Monoid b => a -> (a, b)
    pt x = (x, mempty)

    pe :: Monoid b => a -> (b, a)
    pe x = (mempty, x)

{-|
    Inlines references following the given mapping. If a referenced name cFixot
    be found in the mapping, then it will be left untouched.
    This assumes that the inlined expressions do not contain any free variables.
-}
inlineRefs :: M.Map Name Expr -> Expr -> Expr
inlineRefs scope = foldFix $ \ea -> case ea of
    Ref ref -> fromMaybe (Fix ea) $ scope M.!? ref
    Var {} -> Fix ea
    App ef ex -> Fix $ App ef ex
    TypeApp ef t -> Fix $ TypeApp ef t
    Lam t ex -> Fix $ Lam t ex
    TypeLam ex -> Fix $ TypeLam ex
    InternalExpr {} -> Fix ea

{-|
    Substitutes the first given expression with the given de Bruijn index into
    the second given expression.
-}
substituteExpr :: Int -> Expr -> Expr -> Expr
substituteExpr sidx sea = ($ sea) . ($ sidx) . foldFix subsF
  where
    subsF :: ExprF Type (Int -> Expr -> Expr) -> Int -> Expr -> Expr
    subsF eb idx ea = case eb of
        Ref ref -> Fix $ Ref ref
        Var idx' -> case compare idx' idx of
            LT -> Fix $ Var idx'
            EQ -> ea
            GT -> Fix $ Var (idx' - 1)
        App ef ex -> Fix $ App (ef idx ea) (ex idx ea)
        TypeApp ef t -> Fix $ TypeApp (ef idx ea) t
        Lam t ex -> Fix . Lam t $ ex (idx + 1) (incrementExpr 0 ea)
        TypeLam ex -> Fix . TypeLam $ ex idx ea
        InternalExpr iex -> Fix . InternalExpr $ ($ ea) . ($ idx) <$> iex

{-|
    Substitutes the given type with the given de Bruijn index into the given
    expression.
-}
substituteTypeInExpr :: Int -> Type -> Expr -> Expr
substituteTypeInExpr sidx sta = ($ sta) . ($ sidx) . foldFix subsF
  where
    subsF :: ExprF Type (Int -> Type -> Expr) -> Int -> Type -> Expr
    subsF ea idx ta = case ea of
        Ref ref -> Fix $ Ref ref
        Var idx' -> Fix $ Var idx'
        App ef ex -> Fix $ App (ef idx ta) (ex idx ta)
        TypeApp ef t -> Fix $ TypeApp (ef idx ta) (substituteType idx ta t)
        Lam t ex -> Fix . Lam (substituteType idx ta t) $ ex idx ta
        TypeLam ex -> Fix . TypeLam $ ex (idx + 1) (incrementType 0 ta)
        InternalExpr iex -> Fix . InternalExpr $ ($ ta) . ($ idx) <$> iex

{-|
    Substitutes the first given type with the given de Bruijn index into the
    second given type.
-}
substituteType :: Int -> Type -> Type -> Type
substituteType sidx sta = ($ sta) . ($ sidx) . foldFix subsF
  where
    subsF :: TypeF (Int -> Type -> Type) -> Int -> Type -> Type
    subsF tb idx ta = case tb of
        Arrow tx ty -> Fix $ Arrow (tx idx ta) (ty idx ta)
        Forall tx -> Fix . Forall $ tx (idx + 1) (incrementType 0 ta)
        TypeVar idx' -> case compare idx' idx of
            LT -> Fix $ TypeVar idx'
            EQ -> ta
            GT -> Fix . TypeVar $ idx' - 1
        SpecialType st -> Fix . SpecialType $ ($ ta) . ($ idx) <$> st

{-|
    Increments every 'Var' de Bruijn index greater than or equal to the given
    index.
-}
incrementExpr :: Int -> Expr -> Expr
incrementExpr sidx = ($ sidx) . foldFix incF
  where
    incF :: ExprF Type (Int -> Expr) -> Int -> Expr
    incF ea idx = case ea of
        Ref ref -> Fix $ Ref ref
        Var idx' -> Fix . Var $ if idx' >= idx then idx' + 1 else idx'
        App ef ex -> Fix $ App (ef idx) (ex idx)
        TypeApp ef t -> Fix $ TypeApp (ef idx) t
        Lam t ex -> Fix . Lam t $ ex (idx + 1)
        TypeLam ex -> Fix . TypeLam $ ex idx
        InternalExpr iex -> Fix . InternalExpr $ ($ idx) <$> iex

{-|
    Increments every 'TypeVar' de Bruijn index greater than or equal to the
    given index.
-}
incrementTypeInExpr :: Int -> Expr -> Expr
incrementTypeInExpr sidx = ($ sidx) . foldFix incF
  where
    incF :: ExprF Type (Int -> Expr) -> Int -> Expr
    incF ea idx = case ea of
        Ref ref -> Fix $ Ref ref
        Var idx' -> Fix $ Var idx'
        App ef ex -> Fix $ App (ef idx) (ex idx)
        TypeApp ef t -> Fix $ TypeApp (ef idx) (incrementType idx t)
        Lam t ex -> Fix . Lam (incrementType idx t) $ ex idx
        TypeLam ex -> Fix . TypeLam $ ex (idx + 1)
        InternalExpr iex -> Fix . InternalExpr $ ($ idx) <$> iex

{-|
    Increments every 'TypeVar' de Bruijn index greater than or equal to the
    given index.
-}
incrementType :: Int -> Type -> Type
incrementType sidx = ($ sidx) . foldFix incF
  where
    incF :: TypeF (Int -> Type) -> Int -> Type
    incF ta idx = case ta of
        Arrow tx ty -> Fix $ Arrow (tx idx) (ty idx)
        Forall tx -> Fix . Forall $ tx (idx + 1)
        TypeVar idx' -> Fix . TypeVar $ if idx' >= idx then idx' + 1 else idx'
        SpecialType st -> Fix . SpecialType $ ($ idx) <$> st
