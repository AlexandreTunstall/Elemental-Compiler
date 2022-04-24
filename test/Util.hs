{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}

module Util where

import Control.Monad (unless)
import Data.Bifunctor (Bifunctor, first)
import Data.Fix (Fix(Fix), foldFix, unFix)
import Hedgehog
import Hedgehog.Internal.Property (failWith)

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

-- Hedgehog's tripping requires 'Show' instances.
tripping'
    :: (Eq (f a), Applicative f, MonadTest m)
    => (a -> String) -> (b -> String) -> (f a -> String)
    -> a -> (a -> b) -> (b -> f a) -> m ()
tripping' show1 show2 show3 x enc dec
    = if pure x == my then pure () else failWith Nothing $ unlines
    [ "━━━ Original ━━━"
    , show1 x
    , "━━━ Intermediate ━━━"
    , show2 i
    , "━━━ Roundtrip ━━━"
    , show3 my
    ]
  where
    i = enc x
    my = dec i
