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
import Control.Monad (unless, void)
import Data.Map qualified as M
import Prettyprinter (Pretty(pretty))

import Language.Elemental.Diagnostic
import Language.Elemental.Location
import Language.Elemental.Normalise
import Language.Elemental.Syntax.Internal

-- | A source span maybe annotated with type information.
data TypeInfo = TypeInfo SrcSpan (Maybe (Type SrcSpan))

instance IsLoc TypeInfo where
    getLoc (TypeInfo l _) = getLoc l

{-|
    Checks the type correctness of a declaration and annotates it with type
    information.
    May fail with an error if the type of a referenced name or variable cannot
    be found.
-}
tcDecl
    :: (Has Diagnosis sig m, Has (State (M.Map Name (Type SrcSpan))) sig m)
    => Decl SrcSpan -> m (Decl TypeInfo)
tcDecl decl = case decl of
    Binding l dname@(DeclName _ name) e -> do
        e' <- runReader @[Type SrcSpan] [] $ tcExpr e
        let t = expectType e'
        modify $ M.insert name t
        pure $ Binding (TypeInfo l $ Just t) (noType dname) e'
    ForeignImport l dname@(DeclName _ name) foreignName t -> do
        modify $ M.insert name t
        pure . ForeignImport (TypeInfo l $ Just t) (noType dname) foreignName
            $ noType t
    ForeignExport l foreignName e t -> do
        e' <- runReader @[Type SrcSpan] [] $ tcExpr e
        unless (t ~=~ expectType e')
            $ raise (getLabel e') $ TypeMismatch t (expectType e')
        pure $ ForeignExport (TypeInfo l $ Just t) foreignName e' (noType t)
    ForeignPrimitive l dname@(DeclName _ name) t -> do
        modify $ M.insert name t
        pure $ ForeignPrimitive (TypeInfo l $ Just t) (noType dname) (noType t)

{-|
    Checks the type correctness of an expression and annotates it with type
    information.
    May fail with an error if the type of a referenced name or variable cannot
    be found.
-}
tcExpr
    :: (Has Diagnosis sig m, Has (Reader [Type SrcSpan]) sig m
        , Has (State (M.Map Name (Type SrcSpan))) sig m)
    => Expr SrcSpan -> m (Expr TypeInfo)
tcExpr ea = case ea of
    Ref l name -> do
        t <- gets (M.!? name)
        pure $ Ref (TypeInfo l t) name
    Var l idx -> do
        t <- asks (!? idx)
        pure $ Var (TypeInfo l t) idx
    App l ef ex -> do
        ef' <- tcExpr ef
        ex' <- tcExpr ex
        t <- case expectType ef' of
            Arrow _ tx ty -> if tx ~=~ expectType ex'
                then pure ty
                else raise l $ TypeMismatch tx (expectType ex')
            tx -> raise (getLabel ef') $ TypeExpectedArrow (expectType ex') tx
        pure $ App (TypeInfo l $ Just t) ef' ex'
    TypeApp l ef tx -> do
        ef' <- tcExpr ef
        t <- case expectType ef' of
            Forall _ ty -> pure $ substituteType 0 tx ty
            ty -> raise (getLabel ef') $ TypeExpectedForall ty
        pure $ TypeApp (TypeInfo l $ Just t) ef' (noType tx)
    Lam l tx ey -> do
        ey' <- local (tx :) $ tcExpr ey
        let t = Arrow l tx $ expectType ey'
        pure $ Lam (TypeInfo l $ Just t) (noType tx) ey'
    TypeLam l ex -> do
        ex' <- local @[Type SrcSpan] (incrementType 0 <$>) $ tcExpr ex
        let t = Forall l $ expectType ex'
        pure $ TypeLam (TypeInfo l $ Just t) ex'
    InternalExpr _ _ -> error
        $ "tcExpr: unexpected internal expression: " <> show (pretty ea)
  where
    -- Why isn't this in the Prelude?
    (!?) :: [a] -> Int -> Maybe a
    [] !? _ = Nothing
    (x : _) !? 0 = Just x
    (_ : xs) !? n
        | n > 0 = xs !? pred n
        | otherwise = error "(!?): negative index"

-- | Type equivalence
(~=~) :: Type a -> Type a -> Bool
t1 ~=~ t2 = void t1 == void t2

-- | Extracts the type information from a label, failing if there isn't any.
expectType :: (Labelled f, Pretty (f TypeInfo)) => f TypeInfo -> Type SrcSpan
expectType x = let TypeInfo _ mt = getLabel x in case mt of
    Nothing -> error $ "could not deduce type for " <> show (pretty x)
    Just t -> t

-- | Labels the entire functor with no type information.
noType :: Functor f => f SrcSpan -> f TypeInfo
noType = fmap $ flip TypeInfo Nothing
