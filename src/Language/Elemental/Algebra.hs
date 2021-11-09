{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Elemental.Algebra
    ( -- * Constants
      K(..)
    -- * Sums
    , type (+)(..)
    , eitherS
    , eitherS1
    -- * Products
    , type (*)(..)
    ) where

import Data.Bifunctor (Bifunctor(bimap))
import Data.Data
import Data.Functor.Classes (Eq1(liftEq))
import Data.Kind (Type)


-- | Data family for constant types.
data family K :: Type -> k

newtype instance K a = K { getK :: a }
    deriving stock (Data, Eq, Ord, Read, Show)

newtype instance K a _ = K1 { getK1 :: a }
    deriving stock (Data, Eq, Ord, Read, Show, Foldable, Functor, Traversable)

instance Bifunctor K where
    bimap f _ = K1 . f . getK1

-- | Data family for sum types.
data family (+) :: k -> k -> k
infixr 5 +

data instance a + b = L a | R b
    deriving stock (Data, Eq, Ord, Read, Show)

eitherS :: (a -> r) -> (b -> r) -> a + b -> r
eitherS f _ (L x) = f x
eitherS _ g (R x) = g x
{-# INLINABLE eitherS #-}

data instance (f + g) a = L1 (f a) | R1 (g a)
    deriving stock (Data, Eq, Ord, Read, Show, Foldable, Functor, Traversable)

instance (Eq1 f, Eq1 g) => Eq1 (f + g) where
    liftEq f (L1 x) (L1 y) = liftEq f x y
    liftEq f (R1 x) (R1 y) = liftEq f x y
    liftEq _ _ _ = False

eitherS1 :: (f a -> r) -> (g a -> r) -> (f + g) a -> r
eitherS1 f _ (L1 x) = f x
eitherS1 _ g (R1 x) = g x
{-# INLINABLE eitherS1 #-}

-- | Data family for product types.
data family (*) :: k -> k -> k
infixr 6 *

data instance a * b = P { fstP :: a, sndP :: b }
    deriving stock (Data, Eq, Ord, Read, Show)

data instance (f * g) a = P1 { fstP1 :: f a, sndP1 :: g a }
    deriving stock (Data, Eq, Ord, Read, Show, Foldable, Functor, Traversable)
