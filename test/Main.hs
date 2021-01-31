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
    -- Nix doesn't make it easy to run a test suite with arguments.
    mxml <- lookupEnv "ANT_XML_PATH"
    case mxml of
        Nothing -> runTests
        Just xml -> do
            putStrLn
                $ "The test report will be written to $ANT_XML_PATH = " <> xml
            args <- getArgs
            withArgs ("--xml" : xml : args) runTests

runTests :: IO ()
runTests = do
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
