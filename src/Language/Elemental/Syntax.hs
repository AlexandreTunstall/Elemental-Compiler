{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

-- | Types for representing the Elemental AST and basic manipulation functions.
module Language.Elemental.Syntax
    ( -- * Programs
      Internal.Program
    , mkProgram
    , pattern Program
    -- * AST Types
    , Decl(..)
    , DeclName(..)
    , AnnExpr
    , Expr
    , AnnExprF
    , ExprF(..)
    , AnnType
    , Type
    , AnnTypeF
    , TypeF(..)
    , SpecialType(..)
    , PointerKind(..)
    , Name(..)
    , Internal.InternalExpr
    , InternalType
    -- * Pattern Synonyms
    , pattern (:$:)
    , pattern (:@:)
    , pattern (:\:)
    , pattern (:->:)
    , pattern BitType
    -- * Labelling
    , Labelled(..)
    , annExpr
    , stripExpr
    , annType
    , stripType
    , getBiann
    , getAnn
    , biextract
    , extract
    -- * Pretty-printing
    , module Language.Elemental.Syntax.Pretty
    ) where

import Control.Carrier.State.Church (Has, State, StateC, evalState, get, put)
import Control.Monad (unless, when)
import Data.Fix (foldFix, unFix)
import Data.Foldable (for_, traverse_)
import Data.Graph (SCC(..), stronglyConnComp)
import Data.Map qualified as M
import Data.Text.Short qualified as T

import Language.Elemental.Diagnostic
import Language.Elemental.Emit.Internal
import Language.Elemental.Location
import Language.Elemental.Primitive
import Language.Elemental.Syntax.Internal as External hiding (Program(Program))
import qualified Language.Elemental.Syntax.Internal as Internal
import Language.Elemental.Syntax.Pretty
import Language.Elemental.Syntax.Synonyms
import Language.Elemental.TypeCheck

{-|
    Unidirectional pattern for extracting the declarations from a program.
    To create a program value, see 'mkProgram'.
-}
pattern Program :: a -> [Decl a] -> Internal.Program a
pattern Program l decls <- Internal.Program l decls

{-|
    Creates a program from a list of declarations.
    The declarations are automatically sorted based on dependencies.
    The program is then validated and type checked.
-}
mkProgram
    :: Has Diagnosis sig m
    => SrcSpan -> [Decl SrcSpan] -> m (Internal.Program TypeInfo)
mkProgram l decls = evalHelper $ do
    ordered <- traverse getAcyclic sccs
    traverse_ checkDecl ordered
    Internal.Program (TypeInfo l Nothing) <$> traverse tcDecl ordered
  where
    evalHelper
        :: Applicative m
        => StateC (M.Map Name (Decl SrcSpan)) (StateC (M.Map Name Type) m) a
        -> m a
    evalHelper = evalState M.empty . evalState M.empty

    sccs :: [SCC (Decl SrcSpan)]
    sccs = stronglyConnComp $ toNode <$> decls

    -- Left is foreign name, Right is Elemental name
    toNode
        :: Decl SrcSpan
        -> (Decl SrcSpan, Either T.ShortText Name, [Either T.ShortText Name])
    toNode decl = case decl of
        Binding _ (DeclName _ name) expr
            -> (decl, Right name, Right . snd <$> getReferences expr)
        ForeignImport _ (DeclName _ name) _ _ -> (decl, Right name, [])
        ForeignExport _ foreignName expr _
            -> (decl, Left foreignName, Right . snd <$> getReferences expr)
        ForeignPrimitive _ (DeclName _ name) _ -> (decl, Right name, [])
        ForeignAddress _ (DeclName _ name) _ _ -> (decl, Right name, [])

    checkDecl :: Has Diagnosis sig m => Decl SrcSpan -> m ()
    checkDecl decl = checkPrimitive decl >> checkForeignTypes decl

    checkPrimitive :: Has Diagnosis sig m => Decl SrcSpan -> m ()
    checkPrimitive decl = case decl of
        Binding {} -> pure ()
        ForeignImport {} -> pure ()
        ForeignExport {} -> pure ()
        ForeignPrimitive l' (DeclName _ name) t -> case primitives M.!? name of
            Nothing -> raise (SourceSpan l') $ InvalidPrimitive name
            Just (t', _) -> if stripType t ~=~ t' then pure ()
                else raise (SourceSpan l')
                    $ PrimitiveTypeMismatch name t' (stripType t)
        ForeignAddress {} -> pure ()

    checkForeignTypes :: Has Diagnosis sig m => Decl SrcSpan -> m ()
    checkForeignTypes decl = case decl of
        Binding {} -> pure ()
        ForeignImport _ _ _ t -> checkType t
        ForeignExport _ _ _ t-> checkType t
        ForeignPrimitive {} -> pure ()
        ForeignAddress _ _ _ t -> case toMaybePointerType $ stripType t of
            Nothing -> raise (SourceSpan . getAnn $ unFix t)
                $ IllegalForeignAddressType (stripType t)
            Just _ -> pure ()

    checkType :: Has Diagnosis sig m => AnnType SrcSpan -> m ()
    checkType t = case maybeSplitArrowA t of
        Nothing -> raise (SourceSpan . getAnn $ unFix t)
            $ NonIOForeignType (stripType t)
        Just (targs, tret) -> traverse_ checkArgumentType targs
            >> case toMaybeInternalType $ stripType tret of
                Nothing -> raise (SourceSpan . getAnn $ unFix tret)
                    $ UnmarshallableForeignType (stripType tret)
                Just _ -> pure ()

    checkArgumentType :: Has Diagnosis sig m => AnnType SrcSpan -> m ()
    checkArgumentType targ = case toMaybeArgumentType $ stripType targ of
        Nothing -> raise (SourceSpan . getAnn $ unFix targ)
            $ UnmarshallableForeignType (stripType targ)
        Just _ -> pure ()

    getAcyclic
        :: (Has Diagnosis sig m, Has (State (M.Map Name (Decl SrcSpan))) sig m)
        => SCC (Decl SrcSpan) -> m (Decl SrcSpan)
    getAcyclic comp = case comp of
        AcyclicSCC decl -> decl <$ case decl of
            Binding _ dname expr -> checkExpr expr >> checkName decl dname
            ForeignImport _ dname _ t
                -> checkName decl dname >> checkTypeVars 0 t
            ForeignExport _ _ expr t -> checkExpr expr >> checkTypeVars 0 t
            ForeignPrimitive _ dname t
                -> checkName decl dname >> checkTypeVars 0 t
            ForeignAddress _ dname _ t
                -> checkName decl dname >> checkTypeVars 0 t
        CyclicSCC ds -> raise (SourceMultiple $ SourceSpan . getLabel <$> ds)
            $ CyclicDecls ds

    checkName
        :: (Has Diagnosis sig m, Has (State (M.Map Name a)) sig m)
        => a -> DeclName SrcSpan -> m ()
    checkName v (DeclName l' name) = do
        scope <- get
        when (M.member name scope)
            $ raise (SourceSpan l') $ NameAlreadyInUse name
        put $ M.insert name v scope

    checkExpr
        :: (Has Diagnosis sig m, Has (State (M.Map Name (Decl SrcSpan))) sig m)
        => AnnExpr SrcSpan -> m ()
    checkExpr expr = (checkVars 0 0 expr >>) . for_ (getReferences expr)
        $ \(l', name) -> do
            scope <- get @(M.Map Name (Decl SrcSpan))
            unless (M.member name scope)
                $ raise (SourceSpan l') $ UndefinedRef name

    checkVars :: Has Diagnosis sig m => Int -> Int -> AnnExpr SrcSpan -> m ()
    checkVars sidx stidx = (.) ($ stidx) . (.) ($ sidx) . foldFix
        $ \ea idx tidx -> case biextract ea of
            Ref {} -> pure ()
            Var idx' -> unless (idx > idx')
                $ raise (SourceSpan $ getBiann ea) $ IllegalFreeVar idx idx'
            App ef ex -> ef idx tidx >> ex idx tidx
            TypeApp ef t -> ef idx tidx >> checkTypeVars tidx t
            Lam t ex -> checkTypeVars tidx t >> ex (idx + 1) tidx
            TypeLam ex -> ex idx (tidx + 1)
            InternalExpr {} -> pure ()

    checkTypeVars :: Has Diagnosis sig m => Int -> AnnType SrcSpan -> m ()
    checkTypeVars sidx = (.) ($ sidx) . foldFix $ \ta tidx -> case extract ta of
        Arrow tx ty -> tx tidx >> ty tidx
        Forall tx -> tx $ tidx + 1
        TypeVar tidx' -> unless (tidx > tidx')
            $ raise (SourceSpan $ getAnn ta) $ IllegalFreeTypeVar tidx tidx'
        SpecialType st -> case st of
            IOType tx -> tx tidx
            PointerType _ tx -> tx tidx
            InternalType {} -> pure ()
