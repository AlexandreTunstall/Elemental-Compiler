{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definitions for sending and handling Elemental compiler diagnostics
module Language.Elemental.Diagnostic
    ( Diagnostic(..)
    -- * Sending Diagnostics
    , Diagnosis(..)
    , raise
    , warn
    -- * Handling Diagnostics
    , DiagnosisC
    , runDiagnosis
    , withSource
    ) where

import Control.Algebra (Algebra(alg), Has, send, type (:+:)(L, R))
import Control.Monad.Fix (MonadFix(mfix))
import qualified Data.Kind as K (Type)
import Prettyprinter
    ( Doc
    , align
    , group
    , line
    , nest
    , plural
    , softline
    , vsep
    , (<+>)
    )

import Language.Elemental.Emit.Internal
import Language.Elemental.Location
import Language.Elemental.Syntax.Internal
import Language.Elemental.Syntax.Pretty


-- | An error or warning diagnostic.
data Diagnostic
    -- | The declarations depend on each other.
    = CyclicDecls [Decl SrcSpan]
    -- | The name is already in use.
    | NameAlreadyInUse Name
    -- | The name could not be mapped to a declaration.
    | UndefinedRef Name
    -- | A free variable was used in a top-level expression.
    | IllegalFreeVar Int Int
    -- | A free type variable was used in a top-level expression.
    | IllegalFreeTypeVar Int Int
    -- | The name could not be mapped to a primitive.
    | InvalidPrimitive Name
    -- | The primitive has a different type to the declared type.
    | PrimitiveTypeMismatch Name
        (Type ())
        {-^ Expected -}
        (Type SrcSpan)
        {-^ Actual -}
    -- | The declared foreign type does not have an @IO@ return type.
    | NonIOForeignType (Type SrcSpan)
    -- | The declared foreign type is not a legal marshallable type.
    | UnmarshallableForeignType (Type SrcSpan)
    -- | The declared type does not match the type of the expression.
    | TypeMismatch
        (Type SrcSpan)
        {-^ Expected -}
        (Type SrcSpan)
        {-^ Actual -}
    -- | Application on a non-function type.
    | TypeExpectedArrow
        (Type SrcSpan)
        {-^ The type of the function argument. -}
        (Type SrcSpan)
        {-^ The actual type of the LHS. -}
    -- | Type application on a non-universally quantified type.
    | TypeExpectedForall
        (Type SrcSpan)
        {-^ The actual type of the LHS. -}
    deriving stock (Show)

instance Pretty Diagnostic where
    pretty diag = case diag of
        CyclicDecls decls -> "Cycle in declarations:"
            <> nest 4 (line <> vsep (prettyDeclHead <$> decls))
        NameAlreadyInUse name -> "Name already in use:" <+> pretty name
        UndefinedRef name -> "Undefined reference:" <+> pretty name
            <> line <> "Perhaps the name is misspelt?"
        IllegalFreeVar scope act -> "Illegal use of a free variable."
            <> line <> "The variable " <+> pretty act
            <+> " is not in scope, as there" <+> plural "is" "are" scope
            <+> "only" <+> pretty scope <+> plural "variable" "variables" scope
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
                <+> prettyType0 expect)
            <> line <> group (" But it was declared with type:"
                <+> prettyType0 actual))
        NonIOForeignType t -> "Illegal foreign return type:"
            <+> group (prettyType0 t) <> "." <> line
            <> "Foreign imports and exports must have an IO return type."
        UnmarshallableForeignType t -> "Unmarshallable type:"
            <+> group (prettyType0 t) <> "." <> line
            <> "Foreign imports and exports must have a marshallable type."
            <> line <> "Only booleans" <+> group (prettyType1 $ BitType ())
            <+> "and tuples of booleans are marshallable."
        TypeMismatch expected actual -> "Couldn't match expected type"
            <+> group (prettyType1 expected)
            <+> "with actual type" <+> group (prettyType1 actual) <> "."
        TypeExpectedArrow expArg actual -> "Couldn't match expected type"
            <+> group (prettyType1 $ Arrow () (() <$ expArg) $ TypeVar () 0)
            <+> "with actual type" <+> group (prettyType1 actual)
            <> nest 4 (softline <> "where"
                <+> group (prettyType1 $ TypeVar () 0)
                <+> "is an unknown type.")
            <> line <> "Only a function can be applied to an argument."
        TypeExpectedForall actual -> "Couldn't match expected type"
            <+> group (prettyType1 $ Forall () $ TypeVar () 0)
            <+> "with actual type" <+> group (prettyType1 actual)
            <> nest 4 (softline <> "where"
                <+> group (prettyType1 $ TypeVar () 0)
                <+> "is an unknown type.")
            <> line <> "Only a universally quantified function can be applied\
                \ to an argument."

-- | Prettyprints a 'SourceLocation' at the end of the document.
withSource :: SourceLocation -> Doc ann -> Doc ann
withSource loc doc = doc <> line <> "at" <+> pretty loc

-- | Effect for sending errors and warnings.
data Diagnosis (m :: K.Type -> K.Type) a where
    Raise :: SourceLocation -> Diagnostic -> Diagnosis m a
    Warn :: SourceLocation -> Diagnostic -> Diagnosis m ()

-- | Sends an error at a source location.
raise :: (Has Diagnosis sig m, IsLoc l) => l -> Diagnostic -> m a
raise = (.) send . Raise . getLoc

-- | Sends a warning at a source location.
warn :: (Has Diagnosis sig m, IsLoc l) => l -> Diagnostic -> m ()
warn = (.) send . Warn . getLoc

-- | Carrier for the 'Diagnosis' effect.
newtype DiagnosisC m a = DiagnosisC (forall r. (a -> m r)  -- See [Note DiagC]
    -> (forall x. SourceLocation -> Diagnostic -> m x)
    -> (SourceLocation -> Diagnostic -> m ())
    -> m r)

{-
    [Note DiagC]
    The rank-n type and (a -> m r) argument are essential for 'DiagnosisC' to
    obey the 'Monad' laws. A definition without them deriving via ReaderC would
    not do so, e.g. x >> y would discard the effects of x.
-}

-- | Runs a 'DiagnosisC' carrier.
runDiagnosis
    :: (a -> m r)
    -- ^ Result continuation.
    -> (forall x. SourceLocation -> Diagnostic -> m x)
    -- ^ Error diagnostic handler.
    -> (SourceLocation -> Diagnostic -> m ())
    -- ^ Warning diagnostic handler.
    -> DiagnosisC m a
    -> m r
runDiagnosis p r w (DiagnosisC m) = m p r w

instance Functor (DiagnosisC m) where
    fmap f (DiagnosisC x) = DiagnosisC $ \p r w -> x (p . f) r w

instance Applicative (DiagnosisC m) where
    pure x = DiagnosisC $ \p _ _ -> p x
    DiagnosisC x <*> DiagnosisC y = DiagnosisC
        $ \p r w -> x (\f -> y (p . f) r w) r w

instance Monad (DiagnosisC m) where
    DiagnosisC x >>= f = DiagnosisC $ \p r w -> x (runDiagnosis p r w . f) r w

instance MonadFix m => MonadFix (DiagnosisC m) where
    mfix f = DiagnosisC $ \p r w -> mfix (runDiagnosis pure r w . f) >>= p

instance Algebra sig m => Algebra (Diagnosis :+: sig) (DiagnosisC m) where
    alg hdl sig ctx = DiagnosisC $ \p r w -> case sig of
        L (Raise l diag) -> r l diag -- >>= p . (<$ ctx)
        L (Warn  l diag) -> w l diag >>= p . (<$ ctx)
        R other -> alg (runDiagnosis pure r w . hdl) other ctx >>= p
