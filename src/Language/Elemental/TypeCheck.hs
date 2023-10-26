{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- | Type checking for the Elemental AST.
module Language.Elemental.TypeCheck
    ( tcProgram
    , tcDeclScope
    , tcDecl
    , tcExpr
    , tcType
    , unify
    , unify'
    , unifyPtrKind
    , unifyBackendType
    , unifyNat
    , checkScope
    , checkForeignType
    , checkMarshallableType
    , RefScope
    , RefId(..)
    , succRid
    , Diagnostic
    ) where

import Control.Algebra (Has)
import Data.Fix (foldFix, unFix)
import Data.Graph (SCC(AcyclicSCC, CyclicSCC), stronglyConnComp)
import Data.Kind qualified as Kind
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Type.Equality ((:~:)(Refl))
import Data.Void (absurd)
import Numeric.Natural (Natural)
import Prettyprinter

import Language.Elemental.Algebra
import Language.Elemental.AST.Decl
import Language.Elemental.AST.Expr
import Language.Elemental.AST.Type
import Language.Elemental.AST.Program
import Language.Elemental.AST.Unchecked
import Language.Elemental.Diagnostic
import Language.Elemental.Location
import Language.Elemental.Parser
import Language.Elemental.Pretty
import Language.Elemental.Primitive
import Language.Elemental.Singleton


-- | Type checks a parsed unchecked program.
tcProgram
    :: Has (Diagnosis Diagnostic) sig m
    => PProgram -> m Program
tcProgram (P _ (UProgram decls)) = do
    ordered <- traverse getAcyclic . stronglyConnComp $ node <$> decls
    tcDeclScope M.empty SNil ordered $ pure . Program
  where
    node :: PDecl -> (PDecl, Either ForeignName Name, [Either ForeignName Name])
    node udecl = case sndP udecl of
        UBinding name uexpr -> mkr name $ refs uexpr
        UForeignImport name _ _ -> mkr name []
        UForeignExport fname uexpr _ -> mkl fname $ refs uexpr
        UForeignPrimitive name _ -> mkr name []
        UForeignAddress name _ _ -> mkr name []
      where
        mkr :: AnnName -> [Name]
            -> (PDecl, Either ForeignName Name, [Either ForeignName Name])
        mkr name rs = (udecl, Right $ sndP name, Right <$> rs)

        mkl :: AnnForeignName -> [Name]
            -> (PDecl, Either ForeignName Name, [Either ForeignName Name])
        mkl fname rs = (udecl, Left $ sndP fname, Right <$> rs)

    getAcyclic :: Has (Diagnosis Diagnostic) sig m => SCC PDecl -> m PDecl
    getAcyclic comp = case comp of
        AcyclicSCC decl -> pure decl
        CyclicSCC ds -> raise ds $ CyclicDecls ds
    
    refs :: PExpr -> [Name]
    refs = foldFix $ \uexpr -> case sndP1 uexpr of
        UVar _ -> mempty
        UApp uef uex -> uef <> uex
        UTypeApp uef _ -> uef
        ULam _ uey -> uey
        UTypeLam uex -> uex
        URef name -> pure name

-- | Type checks a parsed unchecked list of declarations.
tcDeclScope
    :: Has (Diagnosis Diagnostic) sig m
    => RefScope scope -> SList (SType 'Zero) scope -> [PDecl]
    -> (forall rest. DeclScope scope rest -> m r) -> m r
tcDeclScope rscope scope udecls cont = case udecls of
    [] -> cont DeclNil
    udecl : udecls' -> tcDecl rscope scope udecl $ \rscope' scope' decl
        -> tcDeclScope rscope' scope' udecls' $ cont . DeclCons decl

-- | Type checks a parsed unchecked declaration.
tcDecl
    :: forall scope sig r m. Has (Diagnosis Diagnostic) sig m
    => RefScope scope -> SList (SType 'Zero) scope
    -> PDecl -> (forall mt. RefScope (MaybeCons mt scope)
        -> SList (SType 'Zero) (MaybeCons mt scope) -> Decl scope mt -> m r)
    -> m r
tcDecl rscope scope udecl cont = case sndP udecl of
    UBinding name uexpr -> tcExpr rscope SZero scope uexpr $ \expr
        -> cont' (rin name) (exprType SZero scope expr :^ scope) $ Binding expr
    UForeignImport name fname utype -> tcType SZero utype $ \t
        -> checkForeignType utype t
        $ cont' (rin name) (t :^ scope) $ ForeignImport (sndP fname) t
    UForeignExport fname uexpr utype -> tcExpr rscope SZero scope uexpr $ \expr
        -> tcType SZero utype $ \t
        -> checkForeignType utype t
        $ unify uexpr t (exprType SZero scope expr)
        $ cont rscope scope $ ForeignExport (sndP fname) expr
    UForeignPrimitive name utype -> tcType SZero utype $ \t
        -> getPrimitive (unName $ sndP name)
            (raise name $ InvalidPrimitive $ sndP name)
            (\pidx -> let texp = exprType SZero SNil $ primitiveExprs !!^ pidx
            in unify' (\_ _ -> raise utype
                $ PrimitiveTypeMismatch (sndP name) texp t
                ) texp t
            $ cont' (rin name) (t :^ scope) $ ForeignPrimitive pidx)
    UForeignAddress name addr utype -> tcType SZero utype $ \t -> case t of
        SPointerType pk tx -> checkMarshallableType
            (raise utype $ IllegalForeignAddressType t) tx
            $ case sIsOpType $ sMarshall tx of
                SFalse -> raise utype $ IllegalForeignAddressType t
                STrue -> cont' (rin name) (t :^ scope)
                    $ ForeignAddress (sndP addr) pk tx
        _ -> raise utype $ IllegalForeignAddressType t
  where
    rin :: AnnName -> m (RefScope (t ': scope))
    rin name = case rscope M.!? sndP name of
        Just _ -> raise name $ NameAlreadyInUse $ sndP name
        _ -> pure $ M.insert (sndP name) (RefId SZero)
            $ succRid <$> rscope
    
    cont' :: forall mt. m (RefScope (MaybeCons mt scope))
        -> SList (SType 'Zero) (MaybeCons mt scope) -> Decl scope mt -> m r
    cont' a b c = a >>= \x -> cont x b c

-- | Type checks a parsed unchecked expression.
tcExpr
    :: Has (Diagnosis Diagnostic) sig m
    => RefScope scope -> SNat tscope -> SList (SType tscope) scope
    -> PExpr -> (forall t. Expr tscope scope t -> m r) -> m r
tcExpr rscope tscope scope uexpr cont = case sndP1 $ unFix uexpr of
    UVar uvar -> checkScope uexpr (sLength scope) uvar $ cont . Var
    UApp uef uex -> tcExpr rscope tscope scope uef $ \ef
        -> tcExpr rscope tscope scope uex $ \ex
        -> case exprType tscope scope ef of
            SArrow tx _ -> unify uex tx (exprType tscope scope ex)
                $ cont $ App ef ex
            tx -> raise uef $ TypeExpectedArrow (exprType tscope scope ex) tx
    UTypeApp uef utx -> tcExpr rscope tscope scope uef $ \ef
        -> tcType tscope utx $ \tx
        -> case exprType tscope scope ef of
            SForall _ -> cont $ TypeApp ef tx
            tf -> raise uef $ TypeExpectedForall tf
    ULam utx uey -> tcType tscope utx $ \tx
        -> tcExpr (succRid <$> rscope) tscope (tx :^ scope) uey $ cont . Lam tx
    UTypeLam uex -> withProof (incAllLen SZero scope)
        $ tcExpr rscope (SSucc tscope) (sIncrementAll tscope SZero scope) uex
        $ cont . TypeLam
    URef name -> case rscope M.!? name of
        Nothing -> raise uexpr $ UndefinedRef name
        Just (RefId ridx) -> cont $ Var ridx

-- | Type checks a parsed unchecked type.
tcType
    :: Has (Diagnosis Diagnostic) sig m
    => SNat tscope
    -> PType -> (forall t. SType tscope t -> m r) -> m r
tcType tscope utype cont = case sndP1 $ unFix utype of
    UTypeVar utvar -> checkScope utype tscope utvar $ cont . STypeVar
    UArrow utx uty -> tcType tscope utx $ \tx -> tcType tscope uty $ \ty
        -> cont $ SArrow tx ty
    UForall utx -> tcType (SSucc tscope) utx $ cont . SForall
    UIOType utx -> tcType tscope utx $ cont . SIOType
    UPointerType upk utx -> tcType tscope utx $ case upk of
        ReadPointer -> cont . SPointerType SReadPointer
        WritePointer -> cont . SPointerType SWritePointer

-- | Attempts to unify two types, producing a proof if they're equal.
unify
    :: forall tscope texp tact loc sig r m. (IsLoc loc
    , Has (Diagnosis Diagnostic) sig m)
    => loc -> SType tscope texp -> SType tscope tact
    -> (texp ~ tact => m r) -> m r
unify loc = unify' $ \ta tb -> raise loc $ TypeMismatch ta tb

-- | 'unify' but the caller has more control over the failing case.
unify'
    :: forall tscope texp tact r. (forall tscope' texp' tact'
        . SType tscope' texp' -> SType tscope' tact' -> r)
    -> SType tscope texp -> SType tscope tact -> (texp ~ tact => r) -> r
unify' mismatch ta tb cont = case (ta, tb) of
    (STypeVar tvaidx, STypeVar tvbidx)
        -> unifyNat (mismatch ta tb) tvaidx tvbidx cont
    (SArrow tax tay, SArrow tbx tby)
        -> unify' mismatch tax tbx $ unify' mismatch tay tby cont
    (SForall tax, SForall tbx) -> unify' mismatch tax tbx cont
    (SIOType tax, SIOType tbx) -> unify' mismatch tax tbx cont
    (SPointerType tapk tax, SPointerType tbpk tbx)
        -> unifyPtrKind (mismatch ta tb) tapk tbpk
        $ unify' mismatch tax tbx cont
    (SBackendType lta, SBackendType ltb)
        -> unifyBackendType (mismatch ta tb) lta ltb cont
    _ -> mismatch ta tb

-- | Attempts to unify two pointer kinds, producing a proof if they're equal.
unifyPtrKind
    :: r -> SPointerKind exp -> SPointerKind act -> (exp ~ act => r) -> r
unifyPtrKind mismatch apk bpk cont = case (apk, bpk) of
    (SReadPointer, SReadPointer) -> cont
    (SWritePointer, SWritePointer) -> cont
    _ -> mismatch

-- | Attempts to unify two t'BackendType', producing a proof if they're equal.
unifyBackendType
    :: r -> SBackendType exp -> SBackendType act -> (exp ~ act => r) -> r
unifyBackendType mismatch lta ltb cont = case (lta, ltb) of
    (SBackendInt sizea, SBackendInt sizeb) -> unifyNat mismatch sizea sizeb cont

-- | Attempts to unify two natural numbers, producing a proof if they're equal.
unifyNat :: r -> SNat exp -> SNat act -> (exp ~ act => r) -> r
unifyNat mismatch a b cont' = case sCmpNat a b of
    SEQ -> withProof (eqToRefl a b Refl) cont'
    _ -> mismatch

-- | Checks that the index is within the scope, producing a proof if it is.
checkScope
    :: forall loc sup sig r m. (IsLoc loc, Has (Diagnosis Diagnostic) sig m)
    => loc -> SNat sup -> Natural
    -> (forall idx. CmpNat idx sup ~ 'LT => SNat idx -> m r) -> m r
checkScope loc sup uidx = go sup uidx
  where
    go :: SNat sup' -> Natural
        -> (forall idx. CmpNat idx sup' ~ 'LT => SNat idx -> m r) -> m r
    go sup' n cont = case sCmpNat sup' SZero of
        SLT -> absurd $ zeroNoLT sup' Refl
        SEQ -> raise loc $ IllegalFreeVar (toNatural sup) (toNatural sup' + n)
        SGT -> withProof (flipCmpNat sup' SZero)
            $ withProof (ltRightPredSucc SZero sup' Refl) $ case n of
                0 -> cont SZero
                _ -> go (sPred sup') (pred n) $ cont . SSucc

-- | Checks that a type is a foreign type, producing a proof if it is.
checkForeignType
    :: forall loc tscope t sig r m. (IsLoc loc
    , Has (Diagnosis Diagnostic) sig m) => loc
    -> SType tscope t -> (HasForeignType t => m r) -> m r
checkForeignType loc t cont = case t of
    SIOType tx -> checkMarshallableType (abort tx) tx cont
    tx :-> ty -> checkMarshallableType (abort tx) tx
        $ case sIsOpType $ sMarshall tx of
            SFalse -> abort tx
            STrue -> checkForeignType loc ty cont
    _ -> raise loc $ NonIOForeignType t
  where
    abort :: SType tscope t' -> m r
    abort t' = raise loc $ UnmarshallableForeignType t'

-- | Checks that a type is a marshallable type, producing a proof if it is.
checkMarshallableType
    :: r -> SType tscope t -> (MarshallableType t => r) -> r
checkMarshallableType abort t cont = case t of
    SForall (STypeVar SZero :-> STypeVar SZero) -> cont
    SBitType -> cont
    SForall ((SBitType :-> tx@(SBitType :-> _)) :-> STypeVar SZero)
        -> fromMaybe abort $ isBitTuple tx cont
    _ -> abort
  where
    isBitTuple :: SType tscope t -> (t ~ BitTuple (ArgCount t) => r) -> Maybe r
    isBitTuple t' x' = case t' of
        STypeVar SZero -> Just x'
        SBitType :-> ty -> isBitTuple ty x'
        _ -> Nothing

-- | Map from names to scope indices.
type RefScope :: [a] -> Kind.Type
type RefScope scope = M.Map Name (RefId (Length scope))

-- | Finite natural numbers. Every inhabitant is less than the type parameter.
type RefId :: Nat -> Kind.Type
data RefId sup where
    RefId :: CmpNat ridx sup ~ 'LT => SNat ridx -> RefId sup

-- | Gets the successor of a 'RefId'.
succRid :: RefId n -> RefId ('Succ n)
succRid (RefId ridx) = RefId (SSucc ridx)

-- | An error or warning diagnostic.
data Diagnostic
    -- | The declarations depend on each other.
    = CyclicDecls [PDecl]
    -- | The name is already in use.
    | NameAlreadyInUse Name
    -- | The name could not be mapped to a declaration.
    | UndefinedRef Name
    -- | A free variable was used in a top-level expression.
    | IllegalFreeVar Natural Natural
    -- | A free type variable was used in a top-level expression.
    | IllegalFreeTypeVar Natural Natural
    -- | The name could not be mapped to a primitive.
    | InvalidPrimitive Name
    -- | The primitive has a different type to the declared type.
    | forall tscope texp tact. PrimitiveTypeMismatch Name
        (SType tscope texp)
        {-^ Expected -}
        (SType tscope tact)
        {-^ Actual -}
    -- | The declared foreign type does not have an @IO@ return type.
    | forall tscope tact. NonIOForeignType (SType tscope tact)
    -- | The declared foreign type is not a legal marshallable type.
    | forall tscope tact. UnmarshallableForeignType (SType tscope tact)
    -- | The declared foreign address is not a marshallable pointer type.
    | forall tscope tact. IllegalForeignAddressType (SType tscope tact)
    -- | The declared type does not match the type of the expression.
    | forall tscope texp tact. TypeMismatch
        (SType tscope texp)
        {-^ Expected -}
        (SType tscope tact)
        {-^ Actual -}
    -- | Application on a non-function type.
    | forall tscope texp tact. TypeExpectedArrow
        (SType tscope texp)
        {-^ The type of the function argument. -}
        (SType tscope tact)
        {-^ The actual type of the LHS. -}
    -- | Type application on a non-universally quantified type.
    | forall tscope tact. TypeExpectedForall
        (SType tscope tact)
        {-^ The actual type of the LHS. -}

instance Pretty Diagnostic where
    pretty diag = case diag of
        CyclicDecls decls -> "Cycle in declarations:"
            <> nest 4 (line <> vsep (prettyPDeclAbbrev <$> decls))
        NameAlreadyInUse name -> "Name already in use:" <+> pretty name
        UndefinedRef name -> "Undefined reference:" <+> pretty name
            <> line <> "Perhaps the name is misspelt?"
        IllegalFreeVar scope act -> "Illegal use of a free variable."
            <> line <> "The variable " <+> pretty act
            <+> " is not in scope, as there"
            <+> plural "is" "are" scope
            <+> "only" <+> pretty scope
            <+> plural "variable" "variables" scope
            <+> "in scope."
        IllegalFreeTypeVar scope act -> "Illegal use of a free type variable."
            <> line <> "The type variable " <+> pretty act
            <+> " is not in scope, as there" <+> plural "is" "are" scope
            <+> "only" <+> pretty scope
            <+> plural "type variable" "type variables" scope <+> "in scope."
        InvalidPrimitive name -> "Invalid primitive:" <+> pretty name <> "."
            <> line <> "A primitive with that name could not be found."
            <> line <> "Perhaps the name is misspelt?"
        PrimitiveTypeMismatch name expect actual
            -> "Mismatching primitive type for" <+> pretty name <> "."
            <> line <> align (group ("The primitive should have type:"
                <+> prettyType 0 expect)
            <> line <> group (" But it was declared with type:"
                <+> prettyType 0 actual))
        NonIOForeignType t -> "Illegal foreign return type:"
            <+> group (prettyType 0 t) <> "." <> line
            <> "Foreign imports and exports must have an IO return type."
        UnmarshallableForeignType t -> "Unmarshallable type:"
            <+> group (prettyType 0 t) <> "." <> line
            <> "Foreign imports and exports must have a marshallable type."
            <> line <> "Only booleans" <+> group (prettyType 1 SBitType)
            <+> "and tuples of booleans are marshallable."
        IllegalForeignAddressType t -> "Illegal foreign address type:"
            <+> group (prettyType 0 t) <> "." <> line
            <> "Foreign addresses must have a marshallable pointer type."
        TypeMismatch expected actual -> "Couldn't match expected type"
            <+> group (prettyType 1 expected)
            <+> "with actual type" <+> group (prettyType 1 actual) <> "."
        TypeExpectedArrow expArg actual -> "Couldn't match expected type"
            <+> group (flip prettyUTypeF 1
                $ UArrow (`prettyType` expArg) $ const placeholder)
            <+> "with actual type" <+> group (prettyType 1 actual)
            <> nest 4
                (softline <> "where" <+> placeholder <+> "is an unknown type.")
            <> line <> "Only a function can be applied to an argument."
        TypeExpectedForall actual -> "Couldn't match expected type"
            <+> group (flip prettyUTypeF 1 . UForall $ const placeholder)
            <+> "with actual type" <+> group (prettyType 1 actual)
            <> nest 4
                (softline <> "where" <+> placeholder <+> "is an unknown type.")
            <> line <> "Only a universally quantified function can be applied\
                \ to an argument."
      where
        placeholder :: Doc ann
        placeholder = "<unknown>"
