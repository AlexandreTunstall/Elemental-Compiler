module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
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
import Location
import Pretty


main :: IO ()
main = do
    -- Generating test reports may fail on Windows if the encoding isn't UTF-8
    setLocaleEncoding utf8
    tests >>= defaultMainWithIngredients (antXMLRunner : defaultIngredients)

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
    timeout = mkTimeout 1000000  -- 1s
