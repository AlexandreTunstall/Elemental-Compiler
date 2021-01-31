{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Functions and effects for implementing rewrite systems.
module Language.Elemental.Rewrite
    ( -- * Rewriter Effect
      Rewriter
    , beginRewrite
    , trackRewrite
    , localRewrite
    -- * Rewriter Carrier
    , RewriterC
    , runRewriter
    -- * Utility Functions
    , rewriteAny
    ) where

import Control.Algebra (Algebra(alg), send, type (:+:)(L, R))
import Control.Carrier.NonDet.Church (Has, NonDet, NonDetC, runNonDetM)
import Control.Effect.Choose (Choose, (<|>))
import Control.Effect.Empty (Empty, empty)
import Control.Monad ((>=>))
import Data.Kind (Type)
import Data.List (inits, tails)
import Data.Monoid (Alt(getAlt))


-- | Effect for tracking rewrites. Useful for debugging rewrite rules.
data Rewriter a (m :: Type -> Type) b where
    TrackRewrite :: a -> Rewriter a m ()
    LocalRewrite :: a -> m s -> Rewriter a m s

-- | Tracks a rewrite.
trackRewrite :: Has (Rewriter a) sig m => a -> m ()
trackRewrite = send . TrackRewrite

-- | Tracks a nested rewrite on a new value.
localRewrite :: Has (Rewriter a) sig m => a -> m s -> m s
localRewrite f x = send $ LocalRewrite f x

{-|
    Rewrites a value by applying the given rewrite rules until the rules fail.
    The local rewrite and resulting rewrites are automatically tracked.
-}
beginRewrite
    :: Has (Rewriter a) sig m
    => (forall n. Algebra (NonDet :+: sig) n => a -> n a)
    -- ^ The rewrite rules to repeatedly apply.
    -> a -> m a
beginRewrite f x = send $ LocalRewrite x $ rewriteLoop runMaybe f x
  where
    runMaybe :: Applicative m => NonDetC m a -> m (Maybe a)
    runMaybe = fmap getAlt . runNonDetM pure

-- | Carrier for the 'Rewriter' effect.
newtype RewriterC a m b = RewriterC (forall r. (b -> m r)
    -> (a -> m ())
    -> (forall s. a -> m s -> m s)
    -> m r
    )

instance Functor (RewriterC a m) where
    fmap f (RewriterC m) = RewriterC $ \k -> m (k . f)

instance Applicative (RewriterC a m) where
    pure x = RewriterC $ \k _ _ -> k x

    RewriterC f <*> RewriterC x = RewriterC
        $ \k t l -> f (\g -> x (k . g) t l) t l

instance Monad (RewriterC a m) where
    RewriterC m >>= f = RewriterC $ \k t l -> m (runRewriter k t l . f) t l

instance Algebra sig m => Algebra (Rewriter a :+: sig) (RewriterC a m) where
    alg hdl sig ctx = RewriterC $ \k t l -> case sig of
        L (TrackRewrite x) -> t x >>= k . (<$ ctx)
        L (LocalRewrite x m) -> l x . runRewriter pure t l . hdl >=> k
            $ m <$ ctx
        R other -> alg (runRewriter pure t l . hdl) other ctx >>= k

-- | Runs a rewriter.
runRewriter
    :: (b -> m r)
    -- ^ Continuation.
    -> (a -> m ())
    -- ^ Rewrite callback.
    -> (forall s. a -> m s -> m s)
    -- ^ New rewrite callback.
    -> RewriterC a m b -> m r
runRewriter cont track local (RewriterC m) = m cont track local

{-|
    Applies rewrite rules repeatedly until they fail.
    The resulting rewrites are automatically tracked.
-}
rewriteLoop
    :: Has (Rewriter a) sig m => (n a -> m (Maybe a)) -> (a -> n a) -> a -> m a
rewriteLoop opt f x = do
    mx' <- opt $ f x
    case mx' of
        Nothing -> pure x
        Just x' -> do
            trackRewrite x'
            rewriteLoop opt f x'

-- | Rewrites the first rewritable value in a list.
rewriteAny
    :: (Has Choose sig m, Has Empty sig m) => (a -> m a) -> ([a] -> m [a])
rewriteAny f xs = foldr (<|>) empty
    $ zipWith3 rebuild (inits xs) (f <$> xs) (drop 1 $ tails xs)
  where
    rebuild :: Functor f => [a] -> f a -> [a] -> f [a]
    rebuild bs x es = (bs <>) . (: es) <$> x
