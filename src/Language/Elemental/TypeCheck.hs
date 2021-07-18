{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

-- | Type checking for the Elemental AST.
module Language.Elemental.TypeCheck
    ( TypeInfo(..)
    , tcDecl
    , (~=~)
    ) where

import Control.Carrier.Reader (Has, Reader, asks, local, runReader)
import Control.Effect.State (State, gets, modify)
import Control.Monad (unless)
import Data.Fix (Fix(Fix), foldFix, unFix)
import Data.Map qualified as M
import Data.Maybe (isJust)

import Language.Elemental.Diagnostic
import Language.Elemental.Location
import Language.Elemental.Normalise
import Language.Elemental.Rewrite
import Language.Elemental.Syntax.Internal
import Language.Elemental.Syntax.Pretty

-- | A source span maybe annotated with type information.
data TypeInfo = TypeInfo SrcSpan (Maybe Type)

instance IsLoc TypeInfo where
    getLoc (TypeInfo l _) = getLoc l

{-|
    Checks the type correctness of a declaration and annotates it with type
    information.
    May fail with an error if the type of a referenced name or variable cannot
    be found.
-}
tcDecl
    :: (Has Diagnosis sig m, Has (State (M.Map Name Type)) sig m)
    => Decl SrcSpan -> m (Decl TypeInfo)
tcDecl decl = case decl of
    Binding l dname@(DeclName _ name) e -> do
        e' <- runReader @[Type] [] $ tcExpr e
        let t = expectType e'
        modify $ M.insert name t
        pure $ Binding (TypeInfo l $ Just t) (lnoType dname) e'
    ForeignImport l dname@(DeclName _ name) foreignName at -> do
        let t = stripType at
        modify $ M.insert name t
        pure . ForeignImport (TypeInfo l $ Just t) (lnoType dname) foreignName
            $ noType at
    ForeignExport l foreignName e at -> do
        let t = stripType at
        e' <- runReader @[Type] [] $ tcExpr e
        unless (t ~=~ expectType e')
            $ raise (getBiann $ unFix e') $ TypeMismatch t (expectType e')
        pure $ ForeignExport (TypeInfo l $ Just t) foreignName e' (noType at)
    ForeignPrimitive l dname@(DeclName _ name) at -> do
        let t = stripType at
        modify $ M.insert name t
        pure
            $ ForeignPrimitive (TypeInfo l $ Just t) (lnoType dname) (noType at)
    ForeignAddress l dname@(DeclName _ name) a at -> do
        let t = stripType at
        modify $ M.insert name t
        pure
            $ ForeignAddress (TypeInfo l $ Just t) (lnoType dname) a (noType at)

{-|
    Checks the type correctness of an expression and annotates it with type
    information.
    May fail with an error if the type of a referenced name or variable cannot
    be found.
-}
tcExpr
    :: (Has Diagnosis sig m, Has (Reader [Type]) sig m
        , Has (State (M.Map Name Type)) sig m)
    => AnnExpr SrcSpan -> m (AnnExpr TypeInfo)
tcExpr = foldFix $ \(Biann l ea) -> case ea of
    Ref name -> do
        t <- gets (M.!? name)
        pure . Fix . Biann (TypeInfo l t) $ Ref name
    Var idx -> do
        t <- asks (!? idx)
        pure . Fix . Biann (TypeInfo l t) $ Var idx
    App ef ex -> do
        ef' <- ef
        ex' <- ex
        t <- case expectType ef' of
            Fix (Arrow tx ty) -> if tx ~=~ expectType ex'
                then pure ty
                else raise l $ TypeMismatch tx (expectType ex')
            tx -> raise (getBiann $ unFix ef')
                $ TypeExpectedArrow (expectType ex') tx
        pure . Fix . Biann (TypeInfo l $ Just t) $ App ef' ex'
    TypeApp ef atx -> do
        let tx = stripType atx
        ef' <- ef
        t <- case expectType ef' of
            Fix (Forall ty) -> pure $ substituteType 0 tx ty
            ty -> raise (getBiann $ unFix ef') $ TypeExpectedForall ty
        pure . Fix . Biann (TypeInfo l $ Just t) $ TypeApp ef' (noType atx)
    Lam atx ey -> do
        let tx = stripType atx
        ey' <- local (tx :) ey
        let t = Fix . Arrow tx $ expectType ey'
        pure . Fix . Biann (TypeInfo l $ Just t) $ Lam (noType atx) ey'
    TypeLam ex -> do
        ex' <- local @[Type] (incrementType 0 <$>) ex
        let t = Fix . Forall $ expectType ex'
        pure . Fix . Biann (TypeInfo l $ Just t) $ TypeLam ex'
    InternalExpr _ -> error $ "tcExpr: unexpected internal expression: " <> show
        (prettyExprF 0 $ const mempty <$ imap (prettyType . stripType) ea)
  where
    -- Why isn't this in the Prelude?
    (!?) :: [a] -> Int -> Maybe a
    [] !? _ = Nothing
    (x : _) !? 0 = Just x
    (_ : xs) !? n
        | n > 0 = xs !? pred n
        | otherwise = error "(!?): negative index"

-- | Type equivalence.
(~=~) :: Type -> Type -> Bool
t1 ~=~ t2 = isJust $ foldFix isEq t1 t2
  where
    isEq :: TypeF (Type -> Maybe ()) -> Type -> Maybe ()
    isEq t1' = match t1' . unFix

-- | Extracts the type information from an annotation, failing if it's absent.
expectType
    :: Pretty (Fix (Biann TypeInfo f g)) => Fix (Biann TypeInfo f g) -> Type
expectType x = let TypeInfo _ mt = getBiann $ unFix x in case mt of
    Nothing -> error $ "could not deduce type for " <> show (pretty x)
    Just t -> t

-- | Reannotates the entire fixed point with no type information.
noType :: Functor f => Fix (Ann SrcSpan f) -> Fix (Ann TypeInfo f)
noType = mapFix . mapAnn $ flip TypeInfo Nothing

-- | Relabels every node with no type information.
lnoType :: Functor f => f SrcSpan -> f TypeInfo
lnoType = fmap (`TypeInfo` Nothing)
