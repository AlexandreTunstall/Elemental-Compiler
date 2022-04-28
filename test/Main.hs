module Main where

import Test.Tasty
    ( TestTree
    , Timeout
    , defaultMain
    , localOption
    , mkTimeout
    , testGroup
    )

import Golden
import Location
import Pretty


main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = do
    goldenTests' <- goldenTests
    pure $ localOption timeout $ testGroup "All"
        [ goldenTests'
        , locationTests
        , prettyTests
        ]
  where
    timeout :: Timeout
    -- timeout = mkTimeout 5000000  -- 5s
    timeout = mkTimeout $ 7 * 86400 * 1000000 -- 7d

