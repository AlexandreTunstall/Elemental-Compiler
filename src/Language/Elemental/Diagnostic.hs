{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definitions for sending and handling diagnostics.
module Language.Elemental.Diagnostic
    ( -- * Sending Diagnostics
      Diagnosis(..)
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
import Prettyprinter (Doc, line, pretty, (<+>))

import Language.Elemental.Location


-- | Prettyprints a 'SourceLocation' at the end of the document.
withSource :: SourceLocation -> Doc ann -> Doc ann
withSource loc doc = doc <> line <> "at" <+> pretty loc

-- | Effect for sending errors and warnings.
data Diagnosis diag (m :: K.Type -> K.Type) a where
    Raise :: SourceLocation -> diag -> Diagnosis diag m a
    Warn :: SourceLocation -> diag -> Diagnosis diag m ()

-- | Sends an error at a source location.
raise :: (Has (Diagnosis diag) sig m, IsLoc l) => l -> diag -> m a
raise = (.) send . Raise . getLoc

-- | Sends a warning at a source location.
warn :: (Has (Diagnosis diag) sig m, IsLoc l) => l -> diag -> m ()
warn = (.) send . Warn . getLoc

-- | Carrier for the 'Diagnosis' effect.
newtype DiagnosisC diag m a = DiagnosisC (forall r
    . (a -> m r)  -- See [Note DiagC]
    -> (forall x. SourceLocation -> diag -> m x)
    -> (SourceLocation -> diag -> m ())
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
    -> (forall x. SourceLocation -> diag -> m x)
    -- ^ Error diagnostic handler.
    -> (SourceLocation -> diag -> m ())
    -- ^ Warning diagnostic handler.
    -> DiagnosisC diag m a
    -> m r
runDiagnosis p r w (DiagnosisC m) = m p r w

instance Functor (DiagnosisC diag m) where
    fmap f (DiagnosisC x) = DiagnosisC $ \p r w -> x (p . f) r w

instance Applicative (DiagnosisC diag m) where
    pure x = DiagnosisC $ \p _ _ -> p x
    DiagnosisC x <*> DiagnosisC y = DiagnosisC
        $ \p r w -> x (\f -> y (p . f) r w) r w

instance Monad (DiagnosisC diag m) where
    DiagnosisC x >>= f = DiagnosisC $ \p r w -> x (runDiagnosis p r w . f) r w

instance MonadFix m => MonadFix (DiagnosisC diag m) where
    mfix f = DiagnosisC $ \p r w -> mfix (runDiagnosis pure r w . f) >>= p

instance Algebra sig m => Algebra (Diagnosis diag :+: sig) (DiagnosisC diag m)
  where
    alg hdl sig ctx = DiagnosisC $ \p r w -> case sig of
        L (Raise l diag) -> r l diag
        L (Warn  l diag) -> w l diag >>= p . (<$ ctx)
        R other -> alg (runDiagnosis pure r w . hdl) other ctx >>= p
