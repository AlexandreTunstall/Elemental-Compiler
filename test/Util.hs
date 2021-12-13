{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}

module Util where

import Control.Monad (unless)
import Data.Bifunctor (Bifunctor, first)
import Data.Fix (Fix(Fix), foldFix, unFix)
import Hedgehog

import Language.Elemental


topLevelAnn :: Fix (K a * f) -> a
topLevelAnn = getK1 . fstP1 . unFix

stripExpr
    :: (Functor g, Functor g', Functor h
        , Functor (f (Fix (g' * h))), Bifunctor f)
    => Fix (g * f (Fix (g' * h))) -> Fix (f (Fix h))
stripExpr = foldFix $ Fix . first stripType . sndP1

stripType :: (Functor g, Functor f) => Fix (g * f) -> Fix f
stripType = foldFix $ Fix . sndP1

(===) :: (Eq a, MonadTest m) => a -> a -> m ()
x === y = unless (x == y) failure

tripping'
    :: (Eq (f a), Applicative f, MonadTest m)
    => a -> (a -> b) -> (b -> f a) -> m ()
tripping' x enc dec = if pure x == my then pure () else failure
  where
    i = enc x
    my = dec i
