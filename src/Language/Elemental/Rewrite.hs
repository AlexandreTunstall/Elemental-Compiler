{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Tools for implementing rewrite systems.
module Language.Elemental.Rewrite
    ( -- * Rewrite Systems
    -- ** Rewriting Functors
      rewrite
    , Rule((:->))
    , RuleIn
    , RuleOut
    , RuleInF(Match, Some)
    , RuleOutF(Static, Dynamic)
    , Matchable(match)
    -- ** Rewriting Bifunctors
    , birewrite
    , Birule((:=>))
    , BiruleIn
    , BiruleOut
    , BiruleInF(Bimatch, Bisome)
    , BiruleOutF(Bistatic, Bidynamic)
    , Bimatchable(bimatch)
    -- ** Utility
    , (!)
    , mapFix
    -- * Debugging Rewrite Rules
    , rewriteM
    , birewriteM
    -- ** Rewriter Effect
    , Rewriter
    , trackRewrite
    , localRewrite
    -- ** Rewriter Carrier
    , RewriterC
    , runRewriter
    -- ** Birewriter Effect
    , Birewriter
    , trackBirewrite
    , localBirewrite
    -- ** Birewriter Carrier
    , BirewriterC
    , runBirewriter
    ) where

import Control.Algebra (Algebra(alg), Has, send, type (:+:)(L, R))
import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor(first))
import Data.Fix (Fix(Fix), foldFix, foldFixM, unFix)
import Data.Foldable (asum)
import Data.IntMap qualified as IM
import Data.Kind (Type)
import Data.Maybe (fromMaybe)


{-|
    A rewrite rule that converts an expression matching the input side into the
    output side. The input side may capture arbitrary state which is aggregated
    and passed to the output side.

    Using this data type instead of functions to describe rewrite rules allows
    for more intelligent application of the rules and better performance of the
    resulting rewrite system.
-}
data Rule f = forall x. Monoid x => RuleIn f x :-> RuleOut f x
infix 0 :->

-- | 'Rule' but for rewriting nested recursive data.
data Birule f g = forall x. Monoid x => BiruleIn f g x :=> BiruleOut f g x
infix 0 :=>

-- | Fixed point of 'RuleInF'.
type RuleIn f x = Fix (RuleInF f x)

-- | Fixed point of 'BiruleInF'.
type BiruleIn f g x = Fix (BiruleInF f g x)

-- | Fixed point of 'RuleOutF'.
type RuleOut f x = Fix (RuleOutF f x)

-- | Fixed point of 'BiruleOutF'.
type BiruleOut f g x = Fix (BiruleOutF f g x)

-- | Functor for rule inputs. This is used to check whether a rule matches.
data RuleInF f x y
    -- | Match a functor as-is.
    = Match (f y)
    -- | Match a fixed point if the given function outputs a capture for it.
    | Some (Fix f -> Maybe x)
    deriving stock (Foldable, Functor, Traversable)

-- | Functor for birule inputs. This is used to check whether a birule matches.
data BiruleInF f g x y
    -- | Match a functor as-is.
    = Bimatch (f (RuleIn g x) y)
    -- | Match a fixed point if the given function outputs a capture for it.
    | Bisome (Fix (f (Fix g)) -> Maybe x)
deriving stock instance Functor (f (RuleIn g x)) => Functor (BiruleInF f g x)
deriving stock instance Foldable (f (RuleIn g x)) => Foldable (BiruleInF f g x)
deriving stock instance Traversable (f (RuleIn g x))
    => Traversable (BiruleInF f g x)

-- | Functor for rule outputs. This is used to derive the output from a match.
data RuleOutF f x y
    -- | Output a functor as-is.
    = Static (f y)
    -- | Output a fixed point dependent on the captures.
    | Dynamic (x -> Fix f)
    deriving stock (Foldable, Functor, Traversable)

-- | Functor for birule outputs. This is used to derive the output from a match.
data BiruleOutF f g x y
    -- | Output a functor as-is.
    = Bistatic (f (RuleOut g x) y)
    -- | Output a fixed point dependent on the captures.
    | Bidynamic (x -> Fix (f (Fix g)))
deriving stock instance Functor (f (RuleOut g x)) => Functor (BiruleOutF f g x)
deriving stock instance Foldable (f (RuleOut g x))
    => Foldable (BiruleOutF f g x)
deriving stock instance Traversable (f (RuleOut g x))
    => Traversable (BiruleOutF f g x)

{-|
    Rewrites a recursive expression and all of its subexpressions using a list
    of rewrite rules.

    The rewrite rules must be terminating, otherwise this function won't always
    terminate. They should also be confluent, as this function makes no
    guarantee as to what rewrite strategy is followed.
-}
rewrite :: forall f. Matchable f => [Rule f] -> Fix f -> Fix f
rewrite rules = foldFix $ rewriteTop . Fix
  where
    rewriteDeep :: Fix f -> Fix f
    rewriteDeep = foldFix $ Fix . fmap rewriteTop

    rewriteTop :: Fix f -> Fix f
    rewriteTop = loopRewrites . (.) asum $ traverse tryRule rules

    tryRule :: Rule f -> Fix f -> Maybe (Fix f)
    tryRule (pat :-> out) x = applyRule out <$> matchRule pat x

    applyRule :: forall x. RuleOut f x -> x -> Fix f
    applyRule out s = foldFix go out
      where
        go :: RuleOutF f x (Fix f) -> Fix f
        go (Static x) = rewriteTop $ Fix x
        go (Dynamic f) = rewriteDeep $ f s

-- | 'rewrite' but it also tracks rewrites via a 'Rewriter'.
rewriteM
    :: forall f m sig. (Traversable f, Matchable f, Has (Rewriter f) sig m)
    => [Rule f] -> Fix f -> m (Fix f)
rewriteM rules = foldFixM (rewriteTop . Fix)
  where
    rewriteDeep :: Fix f -> m (Fix f)
    rewriteDeep = foldFixM $ fmap Fix . traverse rewriteTop

    rewriteTop :: Fix f -> m (Fix f)
    rewriteTop = loopRewritesM localRewrite . (.) asum $ traverse tryRule rules

    tryRule :: Rule f -> Fix f -> Maybe (m (Fix f))
    tryRule rule@(pat :-> out) x = (applyRule out >=> track) <$> matchRule pat x
      where
        track :: Fix f -> m (Fix f)
        track y = y <$ trackRewrite rule y

    applyRule :: forall x. RuleOut f x -> x -> m (Fix f)
    applyRule out s = foldFixM go out
      where
        go :: RuleOutF f x (Fix f) -> m (Fix f)
        go (Static x) = rewriteTop $ Fix x
        go (Dynamic f) = rewriteDeep $ f s

-- | 'rewrite' but for 'Birule'.
birewrite
    :: forall f g. (Bimatchable f, Matchable g)
    => [Birule f g] -> Fix (f (Fix g)) -> Fix (f (Fix g))
birewrite rules = foldFix $ rewriteTop . Fix
  where
    rewriteDeep :: Fix (f (Fix g)) -> Fix (f (Fix g))
    rewriteDeep = foldFix $ Fix . fmap rewriteTop

    rewriteTop :: Fix (f (Fix g)) -> Fix (f (Fix g))
    rewriteTop = loopRewrites . (.) asum $ traverse tryRule rules

    tryRule :: Birule f g -> Fix (f (Fix g)) -> Maybe (Fix (f (Fix g)))
    tryRule (pat :=> out) x = applyRule out <$> bimatchRule pat x

    applyRule :: forall x. BiruleOut f g x -> x -> Fix (f (Fix g))
    applyRule out s = foldFix go out
      where
        go :: BiruleOutF f g x (Fix (f (Fix g))) -> Fix (f (Fix g))
        go (Bistatic x) = rewriteTop $ Fix $ first applyRule' x
        go (Bidynamic f) = rewriteDeep $ f s
    
        applyRule' :: RuleOut g x -> Fix g
        applyRule' = foldFix go'

        go' :: RuleOutF g x (Fix g) -> Fix g
        go' (Static x) = Fix x
        go' (Dynamic f) = f s

-- | 'birewrite' but it also tracks rewrites via a 'Birewriter'.
birewriteM
    :: forall f g m sig.
    ( Traversable (f (Fix g)), forall x. Traversable (f (RuleOut g x))
    , Bimatchable f, Matchable g, Has (Birewriter f g) sig m
    ) => [Birule f g] -> Fix (f (Fix g)) -> m (Fix (f (Fix g)))
birewriteM rules = foldFixM $ rewriteTop . Fix
  where
    rewriteDeep :: Fix (f (Fix g)) -> m (Fix (f (Fix g)))
    rewriteDeep = foldFixM $ fmap Fix . traverse rewriteTop

    rewriteTop :: Fix (f (Fix g)) -> m (Fix (f (Fix g)))
    rewriteTop
        = loopRewritesM localBirewrite . (.) asum $ traverse tryRule rules

    tryRule :: Birule f g -> Fix (f (Fix g)) -> Maybe (m (Fix (f (Fix g))))
    tryRule birule@(pat :=> out) x
        = (applyRule out >=> track) <$> bimatchRule pat x
      where
        track :: Fix (f (Fix g)) -> m (Fix (f (Fix g)))
        track y = y <$ trackBirewrite birule y

    applyRule :: forall x. BiruleOut f g x -> x -> m (Fix (f (Fix g)))
    applyRule out s = foldFixM go out
      where
        go :: BiruleOutF f g x (Fix (f (Fix g))) -> m (Fix (f (Fix g)))
        go (Bistatic x) = pure . Fix $ first applyRule' x
        go (Bidynamic f) = rewriteDeep $ f s

        applyRule' :: RuleOut g x -> Fix g
        applyRule' = foldFix go'

        go' :: RuleOutF g x (Fix g) -> Fix g
        go' (Static x) = Fix x
        go' (Dynamic f) = f s

-- | Attempts to match an expression with a rule.
matchRule
    :: forall f x. (Matchable f, Monoid x) => RuleIn f x -> Fix f -> Maybe x
matchRule = foldFix go
  where
    go :: RuleInF f x (Fix f -> Maybe x) -> Fix f -> Maybe x
    go (Match x) = match x . unFix
    go (Some f) = f

-- | Attempts to match an expression with a birule.
bimatchRule :: forall f g x. (Bimatchable f, Matchable g, Monoid x) => BiruleIn f g x -> Fix (f (Fix g)) -> Maybe x
bimatchRule = foldFix go
  where
    go :: BiruleInF f g x (Fix (f (Fix g)) -> Maybe x) -> Fix (f (Fix g)) -> Maybe x
    go (Bimatch x) = bimatch (first matchRule x) . unFix
    go (Bisome f) = f

-- | Repeats a rewrite function until it fails.
loopRewrites :: forall a. (a -> Maybe a) -> a -> a
loopRewrites f = go
  where
    go :: a -> a
    go x = maybe x go $ f x

-- | Monadic version of 'loopRewrites'.
loopRewritesM
    :: forall a m. Monad m
    => (a -> m a -> m a) -> (a -> Maybe (m a)) -> a -> m a
loopRewritesM act f x = maybe (pure x) (act x . (>>= go)) $ f x
  where
    go :: a -> m a
    go y = maybe (pure y) (>>= go) $ f y

-- | '(IM.!)' but with a nice error message.
(!) :: IM.IntMap a -> Int -> a
vs ! k = fromMaybe abort $ vs IM.!? k
  where
    abort :: a
    abort = error $ "Rewrite.!: key " <> show k <> " is not in "
        <> show (IM.keys vs)

-- | Changes the functor of a 'Fix'.
mapFix :: Functor f => (f (Fix g) -> g (Fix g)) -> Fix f -> Fix g
mapFix f = foldFix $ Fix . f

-- | Class for functors whose structures can be compared.
class Functor f => Matchable f where
    -- | Compares the two structures, outputting 'Just' if they match.
    match :: Monoid b => f (a -> Maybe b) -> f a -> Maybe b

-- | Class for functors with inner structures whose structures can be compared.
class (Bifunctor f, forall x. Functor (f x)) => Bimatchable f where
    -- | Compares the two structures, outputting 'Just' if they match.
    bimatch :: Monoid b => f (a' -> Maybe b) (a -> Maybe b) -> f a' a -> Maybe b

-- | Effect for tracking rewrites. Useful for debugging rewrite rules.
type Rewriter :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data Rewriter f m b where
    TrackRewrite :: Rule f -> Fix f -> Rewriter f m ()
    LocalRewrite :: Fix f -> m s -> Rewriter f m s

-- | Effect for tracking birewrites. Useful for debugging birewrite birules.
type Birewriter
    :: (Type -> Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Type
    -> Type
data Birewriter f g m b where
    TrackBirewrite :: Birule f g -> Fix (f (Fix g)) -> Birewriter f g m ()
    LocalBirewrite :: Fix (f (Fix g)) -> m s -> Birewriter f g m s

-- | Tracks a rewrite.
trackRewrite :: Has (Rewriter f) sig m => Rule f -> Fix f -> m ()
trackRewrite r x = send $ TrackRewrite r x

-- | Tracks a birewrite.
trackBirewrite
    :: Has (Birewriter f g) sig m => Birule f g -> Fix (f (Fix g)) -> m ()
trackBirewrite r x = send $ TrackBirewrite r x

-- | Tracks a nested rewrite on a new value.
localRewrite :: Has (Rewriter f) sig m => Fix f -> m s -> m s
localRewrite x m = send $ LocalRewrite x m

-- | Tracks a nested birewrite on a new value.
localBirewrite :: Has (Birewriter f g) sig m => Fix (f (Fix g)) -> m s -> m s
localBirewrite x m = send $ LocalBirewrite x m

-- | Carrier for the 'Rewriter' effect.
newtype RewriterC f m b = RewriterC (forall r. (b -> m r)
    -> (Rule f -> Fix f -> m ())
    -> (forall s. Fix f -> m s -> m s)
    -> m r
    )

instance Functor (RewriterC f m) where
    fmap f (RewriterC m) = RewriterC $ \k -> m (k . f)
    {-# INLINE fmap #-}

instance Applicative (RewriterC f m) where
    pure x = RewriterC $ \k _ _ -> k x
    {-# INLINE pure #-}

    RewriterC f <*> RewriterC x = RewriterC
        $ \k t l -> f (\g -> x (k . g) t l) t l
    {-# INLINE (<*>) #-}

instance Monad (RewriterC f m) where
    RewriterC m >>= f = RewriterC $ \k t l -> m (runRewriter k t l . f) t l
    {-# INLINE (>>=) #-}

instance Algebra sig m => Algebra (Rewriter f :+: sig) (RewriterC f m) where
    alg hdl sig ctx = RewriterC $ \k t l -> case sig of
        L (TrackRewrite r x) -> t r x >>= k . (<$ ctx)
        L (LocalRewrite x m) -> l x . runRewriter pure t l . hdl >=> k
            $ m <$ ctx
        R other -> alg (runRewriter pure t l . hdl) other ctx >>= k
    {-# INLINE alg #-}

-- | Carrier for the 'Birewriter' effect.
newtype BirewriterC f g m b = BirewriterC (forall r. (b -> m r)
    -> (Birule f g -> Fix (f (Fix g)) -> m ())
    -> (forall s. Fix (f (Fix g)) -> m s -> m s)
    -> m r
    )

instance Functor (BirewriterC f g m) where
    fmap f (BirewriterC m) = BirewriterC $ \k -> m (k . f)
    {-# INLINE fmap #-}

instance Applicative (BirewriterC f g m) where
    pure x = BirewriterC $ \k _ _ -> k x
    {-# INLINE pure #-}

    BirewriterC f <*> BirewriterC x = BirewriterC
        $ \k t l -> f (\g -> x (k . g) t l) t l
    {-# INLINE (<*>) #-}

instance Monad (BirewriterC f g m) where
    BirewriterC m >>= f = BirewriterC $ \k t l -> m (runBirewriter k t l . f) t l
    {-# INLINE (>>=) #-}

instance Algebra sig m => Algebra (Birewriter f g :+: sig) (BirewriterC f g m)
  where
    alg hdl sig ctx = BirewriterC $ \k t l -> case sig of
        L (TrackBirewrite r x) -> t r x >>= k . (<$ ctx)
        L (LocalBirewrite x m) -> l x . runBirewriter pure t l . hdl >=> k
            $ m <$ ctx
        R other -> alg (runBirewriter pure t l . hdl) other ctx >>= k
    {-# INLINE alg #-}

-- | Runs a rewriter.
runRewriter
    :: (b -> m r)
    -- ^ Continuation.
    -> (Rule f -> Fix f -> m ())
    -- ^ Rewrite callback.
    -> (forall s. Fix f -> m s -> m s)
    -- ^ New rewrite callback.
    -> RewriterC f m b -> m r
runRewriter cont track local (RewriterC m) = m cont track local
{-# INLINABLE runRewriter #-}

-- | Runs a birewriter.
runBirewriter
    :: (b -> m r)
    -- ^ Continuation.
    -> (Birule f g -> Fix (f (Fix g)) -> m ())
    -- ^ Birewrite callback.
    -> (forall s. Fix (f (Fix g)) -> m s -> m s)
    -- ^ New rewrite callback.
    -> BirewriterC f g m b -> m r
runBirewriter cont track local (BirewriterC m) = m cont track local
{-# INLINABLE runBirewriter #-}
