{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Elemental.AST.Type
    ( Type(..)
    , SType(..)
    , PointerKind(..)
    , SPointerKind(..)
    , LlvmType(..)
    , SLlvmType(..)
    -- * Synonyms
    , UnitType
    , pattern SUnitType
    , BitType
    , pattern SBitType
    , (:->)
    , pattern (:->)
    -- * Operations
    , Increment
    , sIncrement
    , Substitute
    , sSubstitute
    -- * Proofs
    , incInc
    , incSub
    , subInc
    , subSub
    , subIncElim
    ) where

import Data.Data (Data)
import Data.Kind qualified as Kind
import Data.Proxy (Proxy(Proxy))
import Data.Type.Equality ((:~:)(Refl))
import Data.Void (absurd)
import Unsafe.Coerce qualified as Unsafe

import Language.Elemental.Singleton


-- | The kind of types in the AST.
data Type
    -- | A type variable referencing the type at the given de Bruijn index.
    = TypeVar Nat
    -- | A function from an input type to an output type.
    | Arrow Type Type
    -- | Introduces a type variable to the scope of the contained type.
    | Forall Type
    -- | An impure program description. Used to interact with the outside world.
    | IOType Type
    -- | A pointer to data of the contained type.
    | PointerType PointerKind Type
    -- | An LLVM operand of the given type. This is an internal type.
    | LlvmType LlvmType

{-|
    Singleton for 'Type'. Used to witness an Elemental type at runtime.

    The additional argument indicates the number of type variables in scope.
-}
type SType :: Nat -> Type -> Kind.Type
data SType scope t where
    -- | Singleton constructor for 'TypeVar'.
    STypeVar :: CmpNat idx scope ~ 'LT => SNat idx -> SType scope ('TypeVar idx)
    -- | Singleton constructor for 'Arrow'.
    SArrow :: SType scope tx -> SType scope ty -> SType scope ('Arrow tx ty)
    -- | Singleton constructor for 'Forall'.
    SForall :: SType ('Succ scope) tx -> SType scope ('Forall tx)
    -- | Singleton constructor for 'IOType'.
    SIOType :: SType scope tx -> SType scope ('IOType tx)
    -- | Singleton constructor for 'PointerType'.
    SPointerType
        :: SPointerKind pk -> SType scope tx -> SType scope ('PointerType pk tx)
    -- | Singleton constructor for v'LlvmType'.
    SLlvmType :: SLlvmType lt -> SType scope ('LlvmType lt)

{-|
    The kind of pointer kinds in the AST. Used to annotate pointers with
    information about what pointer operations are legal.
-}
data PointerKind
    -- | Indicates that the pointer can be read.
    = ReadPointer
    -- | Indicates that the pointer can be written.
    | WritePointer
    deriving stock (Data, Eq, Ord, Read, Show)

-- | Singleton for 'SPointerKind'. Used to witness a pointer kind at runtime.
type SPointerKind :: PointerKind -> Kind.Type
data SPointerKind pk where
    -- | Singleton constructor for 'ReadPointer'.
    SReadPointer :: SPointerKind 'ReadPointer
    -- | Singleton constructor for 'WritePointer'.
    SWritePointer :: SPointerKind 'WritePointer

-- | The subset of LLVM types used internally when emitting LLVM.
newtype LlvmType
    -- | An LLVM integer of the given size. A size of 0 is used for @void@.
    = LlvmInt Nat

-- | Singleton for t'LlvmType'. Used to witness an LLVM type at runtime.
type SLlvmType :: LlvmType -> Kind.Type
data SLlvmType lt where
    -- | Singleton constructor for 'LlvmInt'.
    SLlvmInt :: SNat size -> SLlvmType ('LlvmInt size)

-- | The Elemental unit type. Used to encode a @void@ return in the FFI.
type UnitType = 'Forall ('TypeVar 'Zero :-> 'TypeVar 'Zero)

-- | Singleton bidirectional pattern for 'UnitType'.
pattern SUnitType
    :: forall scope t. (CmpNat 'Zero ('Succ scope) ~ 'LT) => (t ~ UnitType)
    => SType scope t
pattern SUnitType <- SForall (STypeVar SZero :-> STypeVar SZero)
  where
    SUnitType = SForall $ STypeVar SZero :-> STypeVar SZero

-- | The Elemental type of booleans. Used to encode bits in the FFI.
type BitType = 'Forall ('TypeVar 'Zero :-> 'TypeVar 'Zero :-> 'TypeVar 'Zero)

-- | Singleton bidirectional pattern for 'BitType'.
pattern SBitType
    :: forall scope t. (CmpNat 'Zero ('Succ scope) ~ 'LT) => (t ~ BitType)
    => SType scope t
pattern SBitType
    <- SForall (STypeVar SZero :-> STypeVar SZero :-> STypeVar SZero)
  where
    SBitType = SForall $ STypeVar SZero :-> STypeVar SZero :-> STypeVar SZero

-- | Infix operator type synonym for 'Arrow'.
type (:->) = 'Arrow

-- | Singleton bidirectional pattern for t'(:->)'.
pattern (:->)
    :: () => (t ~ (:->) tx ty)
    => SType scope tx -> SType scope ty -> SType scope t
pattern tx :-> ty = SArrow tx ty

infixr 2 :->

-- | Introduces a type variable at the given index.
type Increment :: Nat -> Type -> Type
type family Increment idx t where
    Increment idx ('TypeVar tvidx) = 'TypeVar
        (SwitchOrd (CmpNat idx tvidx) ('Succ tvidx) ('Succ tvidx) tvidx)
    Increment idx ('Arrow tx ty) = 'Arrow (Increment idx tx) (Increment idx ty)
    Increment idx ('Forall tx) = 'Forall (Increment ('Succ idx) tx)
    Increment idx ('IOType tx) = 'IOType (Increment idx tx)
    Increment idx ('PointerType pk tx) = 'PointerType pk (Increment idx tx)
    Increment _ ('LlvmType lt) = 'LlvmType lt

-- | Singleton version of 'Increment'.
sIncrement
    :: forall idx scope t. SNat scope -> SNat idx -> SType scope t
    -> SType ('Succ scope) (Increment idx t)
sIncrement scope idx = \case
    STypeVar tvidx
        -> withProof (ltSuccLToLT tvidx (SSucc scope) Refl)
        $ case sCmpNat idx tvidx of
            SLT -> STypeVar $ SSucc tvidx
            SEQ -> STypeVar $ SSucc tvidx
            SGT -> STypeVar tvidx
    SArrow tx ty -> SArrow (sIncrement scope idx tx) (sIncrement scope idx ty)
    SForall tx -> SForall $ sIncrement (SSucc scope) (SSucc idx) tx
    SIOType tx -> SIOType $ sIncrement scope idx tx
    SPointerType pk tx -> SPointerType pk $ sIncrement scope idx tx
    SLlvmType lt -> SLlvmType lt

{-|
    Substitutes a type at the given index. The first type is the substituted
    type and the second type is where the substitutions are made.
-}
type Substitute :: Nat -> Type -> Type -> Type
type family Substitute idx tsub t where
    Substitute idx tsub ('TypeVar tvidx) = SwitchOrd (CmpNat idx tvidx)
        ('TypeVar (Pred tvidx)) tsub ('TypeVar tvidx)
    Substitute idx tsub ('Arrow tx ty)
        = 'Arrow (Substitute idx tsub tx) (Substitute idx tsub ty)
    Substitute idx tsub ('Forall tx)
        = 'Forall (Substitute ('Succ idx) (Increment 'Zero tsub) tx)
    Substitute idx tsub ('IOType tx) = 'IOType (Substitute idx tsub tx)
    Substitute idx tsub ('PointerType pk tx)
        = 'PointerType pk (Substitute idx tsub tx)
    Substitute idx tsub ('LlvmType lt) = 'LlvmType lt

-- | Singleton version of 'Substitute'.
sSubstitute
    :: forall scope idx tsub t. CmpNat idx ('Succ scope) ~ 'LT
    => SNat scope -> SNat idx
    -> SType scope tsub -> SType ('Succ scope) t
    -> SType scope (Substitute idx tsub t)
sSubstitute scope idx tsub = \case
    STypeVar tvidx
        -> withProof (ltRightPredSucc tvidx (SSucc scope) Refl)
        $ case sCmpNat idx tvidx of
            SLT -> withProof (ltRightPredSucc idx tvidx Refl)
                $ STypeVar $ sPred tvidx
            SEQ -> tsub
            SGT -> withProof (flipCmpNat idx tvidx)
                $ withProof (ltTrans tvidx idx scope Refl Refl)
                $ STypeVar tvidx
    SArrow tx ty -> SArrow
        (sSubstitute scope idx tsub tx) (sSubstitute scope idx tsub ty)
    SForall tx -> SForall
        $ sSubstitute (SSucc scope) (SSucc idx) (sIncrement scope SZero tsub) tx
    SIOType tx -> SIOType $ sSubstitute scope idx tsub tx
    SPointerType pk tx -> SPointerType pk $ sSubstitute scope idx tsub tx
    SLlvmType lt -> SLlvmType lt

-- | Proof for transposing two increments.
incInc
    :: forall scope idx1 idx2 t. SNat scope
    -> SNat idx1 -> SNat idx2 -> SType scope t
    -> CmpNat idx2 ('Succ idx1) :~: 'LT
    -> Increment ('Succ idx1) (Increment idx2 t)
        :~: Increment idx2 (Increment idx1 t)
incInc scope idx1 idx2 t lt@Refl = case t of
    STypeVar tvidx -> case sCmpNat idx1 tvidx of
        SLT -> withProof (cmpSuccStepR (SSucc idx1) (SSucc tvidx) Refl)
            $ withProof (ltTrans idx2 (SSucc idx1) (SSucc tvidx) lt Refl)
            $ case sCmpNat idx2 tvidx of
                SLT -> Refl
                SEQ -> Refl
                SGT -> absurd $ ltNoGt idx2 tvidx Refl Refl
        SEQ -> withProof (eqToRefl idx1 tvidx Refl)
            $ withProof (flipCmpNat idx2 tvidx)
            $ case sCmpNat idx2 tvidx of
                SLT -> Refl
                SEQ -> Refl
                SGT -> absurd $ ltNoGt idx2 tvidx Refl Refl
        SGT -> case sCmpNat idx2 tvidx of
            SLT -> Refl
            SEQ -> Refl
            SGT -> withProof (flipCmpNat idx1 tvidx)
                $ withProof (cmpSuccStepR tvidx idx1 Refl)
                $ withProof (flipCmpNat tvidx $ SSucc idx1) Refl
    SArrow tx ty -> withProof (incInc scope idx1 idx2 tx lt)
        $ withProof (incInc scope idx1 idx2 ty lt) Refl
    SForall tx
        -> withProof (incInc (SSucc scope) (SSucc idx1) (SSucc idx2) tx lt) Refl
    SIOType tx -> withProof (incInc scope idx1 idx2 tx lt) Refl
    SPointerType _ tx -> withProof (incInc scope idx1 idx2 tx lt) Refl
    SLlvmType _ -> Refl
{-# RULES "Proof/incInc" incInc
    = \_ _ _ _ -> Unsafe.unsafeCoerce #-}
{-# INLINE [1] incInc #-}

-- | Proof for transposing an increment and a substitution.
incSub
    :: forall sidx iidx scope tsub t. SNat scope
    -> SNat sidx -> SNat iidx -> SType scope tsub -> SType ('Succ scope) t
    -> CmpNat sidx ('Succ iidx) :~: 'LT -> CmpNat iidx ('Succ scope) :~: 'LT
    -> Substitute sidx (Increment iidx tsub) (Increment ('Succ iidx) t)
        :~: Increment iidx (Substitute sidx tsub t)
incSub scope sidx iidx tsub t slt@Refl ilt = case t of
    STypeVar (tvidx :: SNat tvidx) -> case sCmpNat sidx tvidx of
        SLT -> withProof (cmpSuccStepR sidx tvidx Refl)
            $ withProof (ltRightPredSucc sidx tvidx Refl)
            $ case sCmpNat (SSucc iidx) tvidx of
                SLT -> Refl
                SEQ -> Refl
                SGT -> Refl
        SEQ -> withProof (eqToRefl sidx tvidx Refl)
            $ withProof (flipCmpNat sidx $ SSucc iidx) Refl
        SGT -> withProof (flipCmpNat sidx tvidx)
            $ withProof (ltTrans tvidx sidx iidx Refl slt)
            $ withProof (flipCmpNat tvidx iidx)
            $ withProof (flipCmpNat tvidx $ SSucc iidx)
            $ withProof (cmpSuccStepR tvidx iidx Refl) Refl
    SArrow tx ty -> withProof (incSub scope sidx iidx tsub tx slt ilt)
        $ withProof (incSub scope sidx iidx tsub ty slt ilt) Refl
    SForall tx -> withProof (incSub (SSucc scope) (SSucc sidx) (SSucc iidx)
            (sIncrement scope SZero tsub) tx slt ilt)
        $ withProof (incInc scope iidx SZero tsub Refl) Refl
    SIOType tx -> withProof (incSub scope sidx iidx tsub tx slt ilt) Refl
    SPointerType _ tx -> withProof (incSub scope sidx iidx tsub tx slt ilt) Refl
    SLlvmType _ -> Refl
{-# RULES "Proof/incSub" incSub
    = \_ _ _ _ _ _ -> Unsafe.unsafeCoerce #-}
{-# INLINE [1] incSub #-}

-- | Proof for transposing a substitution and an increment.
subInc
    :: forall scope idx1 idx2 tsub t. SNat scope
    -> SNat idx1 -> SNat idx2 -> SType scope tsub -> SType ('Succ scope) t
    -> CmpNat idx1 ('Succ scope) :~: 'LT -> CmpNat idx2 ('Succ idx1) :~: 'LT
    -> Substitute ('Succ idx1) (Increment idx2 tsub) (Increment idx2 t)
        :~: Increment idx2 (Substitute idx1 tsub t)
subInc scope idx1 idx2 tsub t lt1@Refl lt2@Refl = case t of
    STypeVar tvidx -> case sCmpNat idx1 tvidx of
        SLT -> withProof (ltTrans idx2 (SSucc idx1) tvidx lt2 Refl)
            $ withProof (ltRightPredSucc idx2 tvidx Refl)
            $ case sCmpNat idx2 $ sPred tvidx of
                SLT -> Refl
                SEQ -> Refl
                SGT -> absurd $ ltNoGt idx2 (sPred tvidx) Refl Refl
        SEQ -> withProof (eqToRefl idx1 tvidx Refl)
            $ case sCmpNat idx2 idx1 of
                SLT -> Refl
                SEQ -> Refl
                SGT -> absurd $ ltNoGt idx2 idx1 Refl Refl
        SGT -> withProof (flipCmpNat idx1 tvidx)
            $ withProof (cmpSuccStepR tvidx idx1 Refl)
            $ withProof (flipCmpNat tvidx $ SSucc idx1)
            $ case sCmpNat idx2 tvidx of
                SLT -> Refl
                SEQ -> Refl
                SGT -> Refl
    SArrow tx ty -> withProof (subInc scope idx1 idx2 tsub tx lt1 lt2)
        $ withProof (subInc scope idx1 idx2 tsub ty lt1 lt2) Refl
    SForall tx -> withProof (incInc scope idx2 SZero tsub Refl)
        $ withProof (subInc (SSucc scope) (SSucc idx1) (SSucc idx2)
            (sIncrement scope SZero tsub) tx lt1 lt2) Refl
    SIOType tx -> withProof (subInc scope idx1 idx2 tsub tx lt1 lt2) Refl
    SPointerType _ tx -> withProof (subInc scope idx1 idx2 tsub tx lt1 lt2) Refl
    SLlvmType _ -> Refl
{-# RULES "Proof/subInc" subInc
    = \_ _ _ _ _ _ -> Unsafe.unsafeCoerce #-}
{-# INLINE [1] subInc #-}

-- | Proof for transposing two substitutions.
subSub
    :: forall scope idx1 idx2 tsub1 tsub2 t. SNat scope
    -> SNat idx1 -> SNat idx2 -> SType scope tsub1 -> SType ('Succ scope) tsub2
    -> SType ('Succ ('Succ scope)) t
    -> CmpNat idx1 ('Succ idx2) :~: 'LT -> CmpNat idx2 ('Succ scope) :~: 'LT
    -> Substitute idx2 tsub1 (Substitute idx1 tsub2 t)
        :~: Substitute idx1 (Substitute idx2 tsub1 tsub2)
            (Substitute ('Succ idx2) (Increment idx1 tsub1) t)
subSub scope idx1 idx2 tsub1 tsub2 t lt1@Refl lt2@Refl = case t of
    STypeVar tvidx -> case sCmpNat idx1 tvidx of
        SLT -> withProof (ltTrans idx1 (SSucc idx2) (SSucc scope) lt2 Refl)
            $ withProof (ltRightPredSucc idx1 tvidx Refl)
            $ withProof (subIncElim scope idx1
                (Proxy @(Substitute idx2 tsub1 tsub2)) tsub1 Refl)
            $ case sCmpNat (SSucc idx2) tvidx of
                SLT -> withProof (ltTrans idx1 (SSucc idx2) (sPred tvidx)
                    Refl Refl) Refl
                SEQ -> Refl
                SGT -> Refl
        SEQ -> withProof (eqToRefl idx1 tvidx Refl)
            $ withProof (flipCmpNat idx1 $ SSucc idx2) Refl
        SGT -> withProof (flipCmpNat idx1 tvidx)
            $ withProof (ltTrans tvidx idx1 idx2 Refl lt1)
            $ withProof (flipCmpNat tvidx idx2)
            $ withProof (cmpSuccStepR tvidx idx2 Refl)
            $ withProof (flipCmpNat tvidx $ SSucc idx2) Refl
    SArrow tx ty -> withProof (subSub scope idx1 idx2 tsub1 tsub2 tx lt1 lt2)
        $ withProof (subSub scope idx1 idx2 tsub1 tsub2 ty lt1 lt2) Refl
    SForall tx -> withProof (incInc scope idx1 SZero tsub1 Refl)
        $ withProof (subInc scope idx2 SZero tsub1 tsub2 Refl Refl)
        $ withProof (subSub (SSucc scope) (SSucc idx1) (SSucc idx2)
            (sIncrement scope SZero tsub1)
            (sIncrement (SSucc scope) SZero tsub2)
            tx lt1 lt2) Refl
    SIOType tx -> withProof (subSub scope idx1 idx2 tsub1 tsub2 tx lt1 lt2) Refl
    SPointerType _ tx
        -> withProof (subSub scope idx1 idx2 tsub1 tsub2 tx lt1 lt2) Refl
    SLlvmType _ -> Refl
{-# RULES "Proof/subSub" subSub
    = \_ _ _ _ _ _ _ -> Unsafe.unsafeCoerce #-}
{-# INLINE [1] subSub #-}

-- | Proof for eliminating a substition and an increment at the same index.
subIncElim
    :: forall scope idx tsub t proxy. SNat scope
    -> SNat idx -> proxy tsub -> SType scope t
    -> CmpNat idx ('Succ scope) :~: 'LT
    -> Substitute idx tsub (Increment idx t) :~: t
subIncElim scope idx tsub t lt = case t of
    STypeVar tvidx -> case sCmpNat idx tvidx of
        SLT -> withProof (cmpSuccStepR idx tvidx Refl) Refl
        SEQ -> withProof (eqToRefl idx tvidx Refl)
            $ withProof (ltSucc idx) Refl
        SGT -> Refl
    SArrow tx ty -> withProof (subIncElim scope idx tsub tx lt)
        $ withProof (subIncElim scope idx tsub ty lt) Refl
    SForall tx -> withProof (subIncElim (SSucc scope) (SSucc idx)
            (Proxy @(Increment 'Zero tsub)) tx lt) Refl
    SIOType tx -> withProof (subIncElim scope idx tsub tx lt) Refl
    SPointerType _ tx -> withProof (subIncElim scope idx tsub tx lt) Refl
    SLlvmType _ -> Refl
{-# RULES "Proof/subIncElim" subIncElim
    = \_ _ _ _ -> Unsafe.unsafeCoerce #-}
{-# INLINE [1] subIncElim #-}
