{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
    Singletons for working with data kinds.

    Most of the definitions are undocumented because their type or definition
    conveys all the information there is to know about them. If you have trouble
    understanding them, you may find it easier after [learning about
    singletons](https://blog.jle.im/entry/introduction-to-singletons-1.html).
-}
module Language.Elemental.Singleton
    ( Nat(..), SNat, pattern SZero, pattern SSucc, toNatural, withSNat
    , SOrdering(..), CmpNat, sCmpNat
    , module Language.Elemental.Singleton
    ) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import Data.Void (Void, absurd)
import Unsafe.Coerce qualified as Unsafe

import Language.Elemental.Singleton.Unsafe
    ( Nat(..), SNat, pattern SZero, pattern SSucc, toNatural, withSNat
    , SOrdering(..), CmpNat, sCmpNat
    )


sPred :: SNat ('Succ n) -> SNat n
sPred (SSucc n) = n

type Pred :: Nat -> Nat
type family Pred n where
    Pred ('Succ n) = n

type SwitchOrd :: Ordering -> k -> k -> k -> k
type family SwitchOrd ord lt eq gt where
    SwitchOrd 'LT lt _ _ = lt
    SwitchOrd 'EQ _ eq _ = eq
    SwitchOrd 'GT _ _ gt = gt

type FlipOrdering :: Ordering -> Ordering
type family FlipOrdering ord where
    FlipOrdering 'LT = 'GT
    FlipOrdering 'EQ = 'EQ
    FlipOrdering 'GT = 'LT

type SBool :: Bool -> Type
data SBool b where
    SFalse :: SBool 'False
    STrue :: SBool 'True

type (&&) :: Bool -> Bool -> Bool
type family a && b where
    'False && _ = 'False
    'True && b = b

(&&^) :: SBool a -> SBool b -> SBool (a && b)
SFalse &&^ _ = SFalse
STrue &&^ b = b

type If :: Bool -> k -> k -> k
type family If cond t f where
    If 'False _ f = f
    If 'True t _ = t

type SList :: (k -> Type) -> [k] -> Type
data SList sing as where
    SNil :: SList sing '[]
    (:^) :: sing a -> SList sing as -> SList sing (a ': as)
infixr 5 :^

type Length :: [k] -> Nat
type family Length as where
    Length '[] = 'Zero
    Length (_ ': as) = 'Succ (Length as)

sLength :: SList sing as -> SNat (Length as)
sLength SNil = SZero
sLength (_ :^ as) = SSucc $ sLength as

type (!!) :: [k] -> Nat -> k
type family as !! idx where
    (a ': as) !! idx = SwitchOrd (CmpNat idx 'Zero) Stuck a (as !! Pred idx)

(!!^)
    :: CmpNat idx (Length as) ~ 'LT
    => SList sing as -> SNat idx -> sing (as !! idx)
SNil !!^ idx = absurd $ zeroNoLT idx Refl
(a :^ as) !!^ idx = case sCmpNat idx SZero of
    SLT -> absurd $ zeroNoLT idx Refl
    SEQ -> a
    SGT -> withProof (flipCmpNat idx SZero)
        $ withProof (ltRightPredSucc SZero idx Refl)
        $ as !!^ sPred idx

type Insert :: Nat -> k -> [k] -> [k]
type family Insert idx a as where
    Insert _ a '[] = '[a]
    Insert idx a (a' ': as) = SwitchOrd (CmpNat idx 'Zero) Stuck
        (a ': a' ': as) (a' ': Insert (Pred idx) a as)

sInsert :: SNat idx -> sing a -> SList sing as -> SList sing (Insert idx a as)
sInsert _ a SNil = a :^ SNil
sInsert idx a (a' :^ as) = case sCmpNat idx SZero of
    SLT -> absurd $ zeroNoLT idx Refl
    SEQ -> a :^ a' :^ as
    SGT -> withProof (flipCmpNat idx SZero)
        $ withProof (ltRightPredSucc SZero idx Refl)
        $ a' :^ sInsert (sPred idx) a as

type Remove :: Nat -> [k] -> [k]
type family Remove idx as where
    Remove idx (a ': as)
        = SwitchOrd (CmpNat idx 'Zero) Stuck as (a ': Remove (Pred idx) as)

sRemove
    :: CmpNat idx (Length as) ~ 'LT
    => SNat idx -> SList sing as -> SList sing (Remove idx as)
sRemove idx SNil = absurd $ zeroNoLT idx Refl
sRemove idx (a :^ as) = case sCmpNat idx SZero of
    SLT -> absurd $ zeroNoLT idx Refl
    SEQ -> as
    SGT -> withProof (flipCmpNat idx SZero)
        $ withProof (ltRightPredSucc SZero idx Refl)
        $ a :^ sRemove (sPred idx) as

demoteList :: (forall a. sing a -> r) -> SList sing as -> [r]
demoteList _ SNil = []
demoteList f (a :^ as) = f a : demoteList f as

type SMaybe :: (k -> Type) -> Maybe k -> Type
data SMaybe sing ma where
    SNothing :: SMaybe sing 'Nothing
    SJust :: sing a -> SMaybe sing ('Just a)

type FoldMaybe :: kb -> (ka -> kb) -> Maybe ka -> kb
type family FoldMaybe nothing just maybe where
    FoldMaybe nothing _ 'Nothing = nothing
    FoldMaybe _ just ('Just a) = just a

sFoldMaybe
    :: singb nothing -> (forall a. singa a -> singb (just a)) -> SMaybe singa ma
    -> singb (FoldMaybe nothing just ma)
sFoldMaybe nothing _ SNothing = nothing
sFoldMaybe _ just (SJust a) = just a

{-|
    Indicates that a case should never be reached.
    If the compiler reduces a type family application to this, then there should
    be a way of getting a 'Void' value.
    
    No 'Void' in sight means that either a constraint is missing somewhere or
    something has gone terribly wrong.
-}
type family Stuck where

withProof :: a :~: b -> (a ~ b => r) -> r
withProof Refl x = x
{-# INLINE withProof #-}

flipCmpNat :: SNat a -> SNat b -> FlipOrdering (CmpNat a b) :~: CmpNat b a
flipCmpNat SZero SZero = Refl
flipCmpNat SZero (SSucc _) = Refl
flipCmpNat (SSucc _) SZero = Refl
flipCmpNat (SSucc a) (SSucc b) = flipCmpNat a b
{-# RULES "Proof/flipCmpNat" flipCmpNat
    = \_ _ -> Unsafe.unsafeCoerce Refl #-}
{-# INLINE [1] flipCmpNat #-}

cmpSuccStepR
    :: SNat a -> SNat b -> CmpNat a b :~: 'LT -> CmpNat a ('Succ b) :~: 'LT
cmpSuccStepR SZero (SSucc _) Refl = Refl
cmpSuccStepR (SSucc a) (SSucc b) lt = cmpSuccStepR a b lt
{-# RULES "Proof/cmpSuccStepR" cmpSuccStepR
    = \_ _ -> Unsafe.unsafeCoerce #-}
{-# INLINE [1] cmpSuccStepR #-}

ltSucc :: SNat a -> CmpNat a ('Succ a) :~: 'LT
ltSucc SZero = Refl
ltSucc (SSucc a) = ltSucc a
{-# RULES "Proof/ltSucc" ltSucc
    = \_ -> Unsafe.unsafeCoerce Refl #-}
{-# INLINE [1] ltSucc #-}

selfEq :: SNat a -> CmpNat a a :~: 'EQ
selfEq SZero = Refl
selfEq (SSucc a) = selfEq a
{-# RULES "Proof/selfEq" selfEq
    = \_ -> Unsafe.unsafeCoerce Refl #-}
{-# INLINE [1] selfEq #-}

ltSuccLToLT
    :: SNat a -> SNat b -> CmpNat ('Succ a) b :~: 'LT -> CmpNat a b :~: 'LT
ltSuccLToLT SZero (SSucc _) Refl = Refl
ltSuccLToLT (SSucc a) (SSucc b) Refl = ltSuccLToLT a b Refl
{-# RULES "Proof/ltSuccLToLT" ltSuccLToLT
    = \_ _ -> Unsafe.unsafeCoerce #-}
{-# INLINE [1] ltSuccLToLT #-}

ltRightPredSucc
    :: SNat a -> SNat b -> CmpNat a b :~: 'LT -> b :~: 'Succ (Pred b)
ltRightPredSucc a SZero lt = absurd $ zeroNoLT a lt
ltRightPredSucc _ (SSucc _) Refl = Refl
{-# RULES "Proof/ltRightPredSucc" ltRightPredSucc
    = \_ _ -> Unsafe.unsafeCoerce #-}
{-# INLINE [1] ltRightPredSucc #-}

ltTrans
    :: SNat a -> SNat b -> SNat c
    -> CmpNat a b :~: 'LT -> CmpNat b ('Succ c) :~: 'LT -> CmpNat a c :~: 'LT
ltTrans _ (SSucc b) SZero Refl Refl = absurd $ zeroNoLT b Refl
ltTrans SZero (SSucc _) (SSucc _) Refl Refl = Refl
ltTrans (SSucc a) (SSucc b) (SSucc c) Refl Refl = ltTrans a b c Refl Refl
{-# RULES "Proof/ltTrans" ltTrans
    = \_ _ _ _ -> Unsafe.unsafeCoerce #-}
{-# INLINE [1] ltTrans #-}

eqToRefl :: SNat a -> SNat b -> CmpNat a b :~: 'EQ -> a :~: b
eqToRefl SZero SZero Refl = Refl
eqToRefl (SSucc a) (SSucc b) eq = withProof (eqToRefl a b eq) Refl
{-# RULES "Proof/eqToRefl" eqToRefl
    = \_ _ -> Unsafe.unsafeCoerce #-}
{-# INLINE [1] eqToRefl #-}

insZero :: forall a as sing. SList sing as -> Insert 'Zero a as :~: a ': as
insZero SNil = Refl
insZero (_ :^ _) = Refl
{-# INLINE insZero #-}

insZeroP
    :: forall a as sing proxy. proxy a -> SList sing as
    -> Insert 'Zero a as :~: a ': as
insZeroP _ = insZero @a
{-# INLINE insZeroP #-}

insSuccLen
    :: SNat idx -> proxy a -> SList sing as
    -> Length (Insert idx a as) :~: 'Succ (Length as)
insSuccLen _ _ SNil = Refl
insSuccLen idx a (_ :^ as) = case sCmpNat idx SZero of
    SLT -> absurd $ zeroNoLT idx Refl
    SEQ -> Refl
    SGT -> withProof (flipCmpNat idx SZero)
        $ withProof (ltRightPredSucc SZero idx Refl)
        $ withProof (insSuccLen (sPred idx) a as) Refl
{-# RULES "Proof/insSuccLen" insSuccLen
    = \_ _ _ -> Unsafe.unsafeCoerce Refl #-}
{-# INLINE [1] insSuccLen #-}

remPredLen
    :: CmpNat idx (Length as) ~ 'LT
    => SNat idx -> SList sing as
    -> 'Succ (Length (Remove idx as)) :~: Length as
remPredLen idx SNil = absurd $ zeroNoLT idx Refl
remPredLen idx (_ :^ as) = case sCmpNat idx SZero of
    SLT -> absurd $ zeroNoLT idx Refl
    SEQ -> Refl
    SGT -> withProof (flipCmpNat idx SZero)
        $ withProof (ltRightPredSucc SZero idx Refl)
        $ withProof (remPredLen (sPred idx) as) Refl
{-# RULES "Proof/remPredLen" remPredLen
    = \_ _ -> Unsafe.unsafeCoerce Refl #-}
{-# INLINE [1] remPredLen #-}

idxIns
    :: SNat idx -> SNat idx' -> proxy a -> SList sing as
    -> CmpNat idx (Length as) :~: 'LT
    -> as !! idx :~: SwitchOrd (CmpNat idx idx')
        (Insert idx' a as !! idx)
        (Insert idx' a as !! 'Succ idx)
        (Insert idx' a as !! 'Succ idx)
idxIns idx _ _ SNil lt = absurd $ zeroNoLT idx lt
idxIns idx idx' a (_ :^ as) lt = case sCmpNat idx SZero of
    SLT -> absurd $ zeroNoLT idx Refl
    SEQ -> case sCmpNat idx idx' of
        SLT -> withProof (eqToRefl idx SZero Refl)
            $ withProof (flipCmpNat idx idx') Refl
        SEQ -> withProof (eqToRefl idx SZero Refl)
            $ withProof (eqToRefl SZero idx' Refl) Refl
        SGT -> absurd $ withProof (flipCmpNat idx idx')
            $ withProof (eqToRefl idx SZero Refl)
            $ zeroNoLT idx' Refl
    SGT -> case sCmpNat idx idx' of
        SLT -> withProof (flipCmpNat idx SZero)
            $ withProof (ltTrans SZero idx idx' Refl
                $ cmpSuccStepR idx idx' Refl)
            $ withProof (ltRightPredSucc SZero idx Refl)
            $ withProof (ltRightPredSucc SZero idx' Refl)
            $ withProof (idxIns (sPred idx) (sPred idx') a as lt)
            $ withProof (flipCmpNat SZero idx') Refl
        SEQ -> withProof (flipCmpNat idx SZero)
            $ withProof (eqToRefl idx idx' Refl)
            $ withProof (ltRightPredSucc SZero idx Refl)
            $ withProof (idxIns (sPred idx) (sPred idx') a as lt)
            $ withProof (flipCmpNat SZero (SSucc idx'))Refl
        SGT -> case sCmpNat idx' SZero of
            SLT -> absurd $ zeroNoLT idx' Refl
            SEQ -> withProof (flipCmpNat SZero (SSucc idx)) Refl
            SGT -> withProof (flipCmpNat SZero (SSucc idx))
                $ withProof (flipCmpNat idx SZero)
                $ withProof (flipCmpNat idx' SZero)
                $ withProof (ltRightPredSucc SZero idx Refl)
                $ withProof (ltRightPredSucc SZero idx' Refl)
                $ withProof (idxIns (sPred idx) (sPred idx') a as lt) Refl
{-# RULES "Proof/idxIns" idxIns
    = \_ _ _ _ -> Unsafe.unsafeCoerce #-}
{-# INLINE [1] idxIns #-}

idxRem
    :: SNat idx -> SNat idx' -> SList sing as
    -> CmpNat idx (Length as) :~: 'LT
    -> CmpNat idx' (Length as) :~: 'LT
    -> as !! idx :~: SwitchOrd (CmpNat idx idx')
        (Remove idx' as !! idx)
        (as !! idx)
        (Remove idx' as !! Pred idx)
idxRem idx _ SNil lt _ = absurd $ zeroNoLT idx lt
idxRem idx idx' (_ :^ as) lt@Refl lt'@Refl = case sCmpNat idx SZero of
    SLT -> absurd $ zeroNoLT idx Refl
    SEQ -> case sCmpNat idx idx' of
        SLT -> withProof (eqToRefl idx SZero Refl)
            $ withProof (flipCmpNat idx idx') Refl
        SEQ -> withProof (eqToRefl idx SZero Refl)
            $ withProof (eqToRefl SZero idx' Refl) Refl
        SGT -> absurd $ withProof (flipCmpNat idx idx')
            $ withProof (eqToRefl idx SZero Refl)
            $ zeroNoLT idx' Refl
    SGT -> case sCmpNat idx idx' of
        SLT -> withProof (flipCmpNat idx SZero)
            $ withProof (ltTrans SZero idx idx' Refl
                $ cmpSuccStepR idx idx' Refl)
            $ withProof (ltRightPredSucc SZero idx Refl)
            $ withProof (ltRightPredSucc SZero idx' Refl)
            $ withProof (idxRem (sPred idx) (sPred idx') as lt lt')
            $ withProof (flipCmpNat SZero idx') Refl
        SEQ -> Refl
        SGT -> case sCmpNat idx' SZero of
            SLT -> absurd $ zeroNoLT idx' Refl
            SEQ -> Refl
            SGT -> withProof (flipCmpNat idx SZero)
                $ withProof (flipCmpNat idx' SZero)
                $ withProof (ltRightPredSucc SZero idx Refl)
                $ withProof (ltRightPredSucc SZero idx' Refl)
                $ withProof (idxRem (sPred idx) (sPred idx') as lt lt')
                $ withProof (flipCmpNat idx idx')
                $ withProof (ltTrans SZero idx' (sPred idx) Refl Refl)
                $ withProof (flipCmpNat SZero $ sPred idx) Refl
{-# RULES "Proof/idxRem" idxRem
    = \_ _ _ _ -> Unsafe.unsafeCoerce #-}
{-# INLINE [1] idxRem #-}

succNonCyclic :: SNat a -> 'Succ a :~: 'Zero -> Void
succNonCyclic _ = \case {}

ltNoGt
    :: SNat a -> SNat b
    -> CmpNat a ('Succ b) :~: 'LT -> CmpNat a b :~: 'GT -> Void
ltNoGt SZero b Refl Refl = withProof (flipCmpNat SZero b) $ zeroNoLT b Refl
ltNoGt (SSucc a) SZero Refl Refl = case a of {}
ltNoGt (SSucc a) (SSucc b) Refl Refl = ltNoGt a b Refl Refl

gtNoLt
    :: SNat a -> SNat b
    -> CmpNat a b :~: 'GT -> CmpNat ('Succ a) b :~: 'LT -> Void
gtNoLt SZero b Refl Refl = withProof (flipCmpNat SZero b) $ zeroNoLT b Refl
gtNoLt (SSucc a) (SSucc b) Refl Refl = gtNoLt a b Refl Refl

zeroNoLT :: SNat a -> CmpNat a 'Zero :~: 'LT -> Void
zeroNoLT a lt = case (a, lt) of {}
