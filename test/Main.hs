module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment
import Test.Tasty
    ( TestTree
    , Timeout
    , defaultIngredients
    , defaultMainWithIngredients
    , localOption
    , mkTimeout
    , testGroup
    )
import Test.Tasty.Runners.AntXML (antXMLRunner)

import Golden


main :: IO ()
main = do
    -- Generating test reports may fail on Windows if the encoding isn't UTF-8
    setLocaleEncoding utf8
    tests >>= defaultMainWithIngredients (antXMLRunner : defaultIngredients)

tests :: IO TestTree
tests = do
    goldenTests' <- goldenTests
    pure $ localOption timeout $ testGroup "All tests"
        [ goldenTests'
        ]
  where
    timeout :: Timeout
    timeout = mkTimeout 1000000  -- 1s
