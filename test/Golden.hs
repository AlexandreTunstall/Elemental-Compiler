{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Golden where

import Control.Carrier.Reader (Reader, ReaderC, ask, local, runReader)
import Control.Effect.Lift (Has, Lift, run, sendIO)
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
import Prettyprinter (Doc, PageWidth(Unbounded), defaultLayoutOptions, group, layoutPageWidth, layoutPretty, (<+>))
import Prettyprinter.Render.String (renderString)
import System.FilePath (replaceExtension, takeBaseName)
import System.IO (Handle, IOMode(WriteMode), hPutStrLn, withFile)
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
    (loc, decls) <- parseSource src
    let prog = printDiags $ mkProgram loc decls
    llvmDefs <- withFile (replaceExtension file ".log") WriteMode
        $ \h -> printRewrites h $ emitProgram prog
    let llvm = defaultModule
            { moduleSourceFileName = TS.toShortByteString $ TS.fromString file
            , moduleDefinitions = llvmDefs
            }
    withContext $ \ctx -> withModuleFromAST ctx llvm
        $ \m -> withPassManager passes $ \pm -> do
            writeLLVMAssemblyToFile (File $ replaceExtension file ".ll") m
            verify m
            _ <- runPassManager pm m
            BSL.fromStrict <$> moduleLLVMAssembly m
  where
    parseSource :: T.Text -> IO (SrcSpan, [Decl SrcSpan])
    parseSource src = case runParser (mkParser $ pProgram <* eof) file src of
        Left errors -> error $ errorBundlePretty errors
        Right decls -> pure decls

printDiags :: DiagnosisC Identity a -> a
printDiags = run . runDiagnosis pure (printDiag "Error") (printDiag "Warning")

printDiag :: String -> SourceLocation -> Diagnostic -> r
printDiag t l d = error $ show $ pretty t <> ":" <+> withSource l (pretty d)

printRewrites :: Handle -> RewriterC (Expr TypeInfo) (ReaderC Int IO) a -> IO a
printRewrites h = runReader (-1) . runRewriter pure printRewrite wrapRewrite
  where
    printRewrite
        :: (Has (Reader Int) sig m, Has (Lift IO) sig m)
        => Expr TypeInfo -> m ()
    printRewrite expr = putIndented
        $ "==== into " <> showDoc (prettyExpr0 expr)

    wrapRewrite
        :: (Has (Reader Int) sig m, Has (Lift IO) sig m)
        => Expr TypeInfo -> m a -> m a
    wrapRewrite expr m = local @Int (+1)
        $ putIndented ("Rewriting " <> showDoc (prettyExpr0 expr)) >> m

    putIndented
        :: (Has (Reader Int) sig m, Has (Lift IO) sig m) => String -> m ()
    putIndented str = do
        depth <- ask
        sendIO . hPutStrLn h . foldr (:) str $ replicate (4 * depth) ' '
    
    showDoc :: Doc ann -> String
    showDoc = renderString . layoutPretty layoutOpts . group
      where
        layoutOpts = defaultLayoutOptions
            { layoutPageWidth = Unbounded
            }

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
