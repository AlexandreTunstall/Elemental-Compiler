{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK not-home #-}

{-|
    The unsafe internals of "Language.Elemental.Singleton".
    Misuse will lead to heap corruption and other nasty unpredictable effects.
    Use at your own risk!

    The safe definitions are re-exported in "Language.Elemental.Singleton". If
    you do not need to use anything here documented as unsafe, then import that
    instead.
-}
module Language.Elemental.Singleton.Unsafe
    ( module Language.Elemental.Singleton.Unsafe
    ) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)
import Unsafe.Coerce qualified as Unsafe

{-
    A typical Peano-based singleton gives poor memory performance when compiling
    large programs. Here, we define the singleton based on Natural using
    unsafeCoerce instead.

    GHC's Nat is very inconvenient for type-level programming because it does
    not allow us to derive even basic laws like @KnownNat n => KnownNat (n + 1)@
    without using type solver plugins, which tend to be less stable than GHC.
    Hence, we can't let GHC handle the unsafe coercions for us either.
-}

-- | Natural numbers based on the Peano construction.
data Nat = Zero | Succ Nat

-- | Singleton for 'Nat'. Used to witness a natural number at runtime.
type SNat :: Nat -> Type
data SNat n where
    {-|
        Unsafe! If used, care must be taken to ensure the type matches the
        value contained, otherwise nasty things will happen.

        You most likely don't need to use this, because most uses of 'SNat' can
        be proven to be correct without needing to resort to manually hacking
        this internal representation.
    -}
    SNat :: Natural -> SNat n

{-|
    Singleton for 'Nat'. Unlike 'SNat', this allows pattern matching with the
    Peano construction. 'SNat' isn't defined to use the Peano construction to
    achieve better performance.
-}
type IsZeroOrSucc :: Nat -> Type
data IsZeroOrSucc n where
    IsZero :: IsZeroOrSucc 'Zero
    IsSucc :: SNat n -> IsZeroOrSucc ('Succ n)

-- | Singleton bidirectional pattern synonym for 'Zero'.
pattern SZero :: () => (n ~ 'Zero) => SNat n
pattern SZero <- (matchNat -> IsZero)
  where
    SZero = SNat 0

-- | Singleton bidirectional pattern synonym for 'Succ'.
pattern SSucc :: () => (n ~ 'Succ n') => SNat n' -> SNat n
pattern SSucc n <- (matchNat -> IsSucc n)
  where
    SSucc (SNat n) = SNat $ succ n

{-# COMPLETE SZero, SSucc #-}

{-|
    Converts a singleton to a natural number.

    This function gives better performance than manually folding.
-}
toNatural :: SNat n -> Natural
toNatural (SNat n) = n

{-|
    Converts a natural number to a singleton.

    This function gives better performance than manually folding.
-}
withSNat :: Natural -> (forall n. SNat n -> r) -> r
withSNat n cont = cont $ SNat n

{-|
    Converts an 'SNat' into an 'IsZeroOrSucc' to allow pattern matching.

    You probably don't need to use this, because you can pattern match on 'SNat'
    directly with the 'SZero' and 'SSucc' bidirectional pattern synonyms.
-}
matchNat :: forall n. SNat n -> IsZeroOrSucc n
matchNat (SNat n) = case n of
    0 -> case Unsafe.unsafeCoerce @_ @(n :~: 'Zero) Refl of
        Refl -> IsZero
    _ -> case Unsafe.unsafeCoerce @_ @(n :~: 'Succ _) Refl of
        Refl -> IsSucc (SNat $ pred n)

-- | Singleton for 'Ordering'. Used to witness an ordering at runtime.
type SOrdering :: Ordering -> Type
data SOrdering ord where
    SLT :: SOrdering 'LT
    SEQ :: SOrdering 'EQ
    SGT :: SOrdering 'GT

-- | Compares two natural numbers.
type CmpNat :: Nat -> Nat -> Ordering
type family CmpNat a b where
    CmpNat 'Zero 'Zero = 'EQ
    CmpNat 'Zero ('Succ _) = 'LT
    CmpNat ('Succ _) 'Zero = 'GT
    CmpNat ('Succ a) ('Succ b) = CmpNat a b

{-|
    Singleton version of 'CmpNat'.

    This function gives better performance than manually folding.
-}
sCmpNat :: SNat a -> SNat b -> SOrdering (CmpNat a b)
sCmpNat (SNat a) (SNat b) = case compare a b of
    LT -> Unsafe.unsafeCoerce SLT
    EQ -> Unsafe.unsafeCoerce SEQ
    GT -> Unsafe.unsafeCoerce SGT
