{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Golden where

import Control.Algebra (Algebra(alg), Has, (:+:)(L, R), run)
import Control.Carrier.Reader (ReaderC(ReaderC), runReader)
import Control.Carrier.State.Church (State, evalState, get, gets, modify)
import Control.Lens (Iso', iso, ix, (^?), (%~))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
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
import Prettyprinter
    ( Doc, PageWidth(Unbounded)
    , defaultLayoutOptions, layoutPageWidth, layoutPretty, line, nest, pretty, (<+>)
    )
import Prettyprinter.Render.Text (renderIO)
import System.FilePath (replaceExtension, takeBaseName)
import System.IO
    ( BufferMode(NoBuffering), Handle, IOMode(WriteMode)
    , hPrint, hPutStrLn, hSetBuffering, withFile
    )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Text.Megaparsec (MonadParsec(eof), errorBundlePretty, runParser)

import Language.Elemental hiding (L, R)
import Language.Elemental.Backend.LLVM


goldenTests :: IO TestTree
goldenTests = do
    elemFiles <- findByExtension [".elem"] "test/Golden"
    pure $ testGroup "Golden tests"
        [ goldenVsString (takeBaseName elemFile) llvmFile $ compileFile elemFile
        | elemFile <- elemFiles
        , let llvmFile = replaceExtension elemFile ".opt.ll"
        ]

compileFile :: FilePath -> IO BSL.ByteString
compileFile file = runGolden $ \lh -> do
    uprog <- liftIO $ do
        hSetBuffering lh NoBuffering
        src <- TE.decodeUtf8 <$> BS.readFile file
        parseSource src
    let prog = printDiags $ tcProgram uprog
    liftIO $ hPutStrLn lh "Emitting"
    exts <- emitProgram prog
    graph <- get @INet
    liftIO $ withFile (replaceExtension file ".inet") WriteMode
        $ \h -> hPutDoc h $ pretty graph
    liftIO $ hPutStrLn lh "Interpreting"
    gen <- compileINet exts
    graph' <- get @INet
    liftIO $ hPutStrLn lh "Translating"
    let llvmDefs = compileProgram gen
        llvm = defaultModule
            { moduleSourceFileName = TS.toShortByteString $ TS.fromString file
            , moduleDefinitions = llvmDefs
            }
    liftIO $ do
        withFile (replaceExtension file ".opt.inet") WriteMode
            $ \h -> hPutDoc h $ pretty graph'
        withFile (replaceExtension file ".hl") WriteMode
            $ \h -> hPutDoc h $ pretty gen
        withContext $ \ctx -> withModuleFromAST ctx llvm
            $ \m -> withPassManager passes $ \pm -> do
                -- writeLLVMAssemblyToFile doesn't truncate the file.
                () <- withFile (replaceExtension file ".ll") WriteMode mempty
                writeLLVMAssemblyToFile (File $ replaceExtension file ".ll") m
                verify m
                {-
                    Run -O3 multiple times because llvm-hs doesn't allow us to
                    build our own custom pipeline with all the passes we need
                    and once isn't enough.
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

    runGolden m = withFile (replaceExtension file ".log") WriteMode $ \lh
        -> evalState @INet mempty
        $ evalState @INetPairs mempty
        $ evalState @INetSize 0
        $ evalState @Count 0
        $ runReader @Level 0
        $ runTraceRewrite <*> m $ lh

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

newtype TraceRewriteC m a = TraceRewriteC (Handle -> m a)
    deriving (Functor, Applicative, Monad, MonadIO) via ReaderC Handle m

runTraceRewrite :: Handle -> TraceRewriteC m a -> m a
runTraceRewrite h (TraceRewriteC f) = f h
{-# INLINE runTraceRewrite #-}

instance (MonadIO m, Has (State Count) sig m, Has (State INet) sig m
    , Has (State INetPairs) sig m, Has (State INetSize) sig m)
    => Algebra (TraceRewrite :+: sig) (TraceRewriteC m)
  where
    alg hdl sig ctx = TraceRewriteC $ \h -> case sig of
        L (TraceRewrite _ _ n0 n1 cont) -> do
            size0 <- gets unINetSize
            r <- runTraceRewrite h . hdl $ cont <$ ctx
            lint size0 n0 n1
            modify $ _Count %~ succ
            count <- gets unCount
            netSize <- gets (IM.size . unINet)
            pairsSize <- gets (IS.size . unINetPairs)
            liftIO $ hPrint h
                $ pretty count
                <+> pretty netSize
                <+> pretty pairsSize
                <+> pretty size0
                <> nest 4 (line <> pretty n0 <> line <> pretty n1)
            pure r
        R other -> alg (runTraceRewrite h . hdl) other ctx
      where
        lint :: Has (State INet) sig m => Int -> INetF Ref -> INetF Ref -> m ()
        lint size n0 n1
            = gets unINet >>= (traverse_ . traverse) lintRef
          where
            lintRef :: Has (State INet) sig m => Ref -> m ()
            lintRef (Ref (-1) (-1)) = abort "uninitialised ref"
            lintRef r3 = do
                net <- get @INet
                case net ^? ix (refNode r3) of
                    Nothing -> abort $ "missing node:" <+> pretty r3
                    Just n4 -> case n4 ^? ix (refPort r3) of
                        Nothing -> abort $ "missing port:" <+> pretty r3
                        Just _ -> pure ()

            abort :: Has (State INet) sig m => Doc ann -> m a
            abort msg = do
                net <- get @INet
                error . show $ "lint:" <+> msg
                    <> line <> "Size before reduction was" <+> pretty size
                    <> line <> "Node 1: " <> pretty n0
                    <> line <> "Node 2: " <> pretty n1
                    <> line <> pretty net
    {-# INLINE alg #-}

newtype Count = Count { unCount :: Int }
    deriving newtype (Eq, Num, Ord)

_Count :: Iso' Count Int
_Count = iso unCount Count
{-# INLINE _Count #-}

hPutDoc :: Handle -> Doc ann -> IO ()
hPutDoc h doc = renderIO h $ layoutPretty opts doc
  where
    opts = defaultLayoutOptions
        { layoutPageWidth = Unbounded
        }

