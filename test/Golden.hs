{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Golden where

import Control.Algebra (run)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Functor.Identity (Identity)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Short qualified as TS
import LLVM.AST (Module(moduleDefinitions, moduleSourceFileName), defaultModule)
import LLVM.Analysis (verify)
import LLVM.Context (withContext)
import LLVM.Module
    (File(File), moduleLLVMAssembly, withModuleFromAST, writeLLVMAssemblyToFile)
import LLVM.PassManager (runPassManager, withPassManager)
import LLVM.PassManager qualified as LLVM.Pass
import Prettyprinter (pretty, (<+>))
import System.FilePath (replaceExtension, takeBaseName)
import System.IO (IOMode(WriteMode), withFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Text.Megaparsec (MonadParsec(eof), errorBundlePretty, runParser)

import Language.Elemental


goldenTests :: IO TestTree
goldenTests = do
    elemFiles <- findByExtension [".elem"] "test/Golden"
    pure $ testGroup "Golden tests"
        [ goldenVsString (takeBaseName elemFile) llvmFile $ compileFile elemFile
        | elemFile <- elemFiles
        , let llvmFile = replaceExtension elemFile ".opt.ll"
        ]

compileFile :: FilePath -> IO BSL.ByteString
compileFile file = do
    src <- TE.decodeUtf8 <$> BS.readFile file
    uprog <- parseSource src
    let prog = printDiags $ tcProgram uprog
        llvmDefs = emitProgram prog
        llvm = defaultModule
            { moduleSourceFileName = TS.toShortByteString $ TS.fromString file
            , moduleDefinitions = llvmDefs
            }
    withContext $ \ctx -> withModuleFromAST ctx llvm
        $ \m -> withPassManager passes $ \pm -> do
            -- writeLLVMAssemblyToFile doesn't truncate the file if it exists.
            () <- withFile (replaceExtension file ".ll") WriteMode mempty
            writeLLVMAssemblyToFile (File $ replaceExtension file ".ll") m
            verify m
            {-
                Run -O3 multiple times because llvm-hs doesn't allow us to build
                our own custom pipeline with all the passes we need and once
                isn't enough.
            -}
            _ <- runPassManager pm m
            _ <- runPassManager pm m
            _ <- runPassManager pm m
            _ <- runPassManager pm m
            _ <- runPassManager pm m
            BSL.fromStrict <$> moduleLLVMAssembly m
  where
    parseSource :: T.Text -> IO PProgram
    parseSource src = case runParser (mkParser $ pProgram <* eof) file src of
        Left errors -> error $ errorBundlePretty errors
        Right uprog -> pure uprog

printDiags :: DiagnosisC Diagnostic Identity a -> a
printDiags = run . runDiagnosis pure (printDiag "Error") (printDiag "Warning")

printDiag :: String -> SourceLocation -> Diagnostic -> r
printDiag t l d = error $ show $ pretty t <> ":" <+> withSource l (pretty d)

passes :: LLVM.Pass.PassSetSpec
passes = LLVM.Pass.CuratedPassSetSpec
    { LLVM.Pass.optLevel = Just 3
    , LLVM.Pass.sizeLevel = Nothing
    , LLVM.Pass.unitAtATime = Nothing
    , LLVM.Pass.simplifyLibCalls = Nothing
    , LLVM.Pass.loopVectorize = Nothing
    , LLVM.Pass.superwordLevelParallelismVectorize = Nothing
    , LLVM.Pass.useInlinerWithThreshold = Nothing
    , LLVM.Pass.dataLayout = Nothing
    , LLVM.Pass.targetLibraryInfo = Nothing
    , LLVM.Pass.targetMachine = Nothing
    }
