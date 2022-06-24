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
import Control.Carrier.State.Church
    (State, evalState, get, gets, modify, runState)
import Control.Lens (Fold, Iso', at, iso, ix, to, (^?), (%~), (^..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as M
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
import LLVM.Transforms qualified as LLVM.Opt
import Prettyprinter
    ( Doc, PageWidth(Unbounded), Pretty
    , defaultLayoutOptions, layoutPageWidth, layoutPretty
    , line, nest, pretty, vcat, (<+>)
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
    bprog <- compileINet exts
    graph' <- get @INet
    liftIO $ hPutStrLn lh "Translating"
    let llvmDefs = compileProgram bprog
        llvm = defaultModule
            { moduleSourceFileName = TS.toShortByteString $ TS.fromString file
            , moduleDefinitions = llvmDefs
            }
    liftIO $ do
        withFile (replaceExtension file ".opt.inet") WriteMode
            $ \h -> hPutDoc h $ pretty graph'
        withFile (replaceExtension file ".hl") WriteMode
            $ \h -> hPutDoc h $ pretty bprog
        withContext $ \ctx -> withModuleFromAST ctx llvm
            $ \m -> withPassManager passes $ \pm
            -> withPassManager passes' $ \pm' -> do
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
                _ <- runPassManager pm' m
                _ <- runPassManager pm m
                _ <- runPassManager pm m
                _ <- runPassManager pm' m
                _ <- runPassManager pm m
                _ <- runPassManager pm m
                _ <- runPassManager pm' m
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
        $ runState @Count (flip (<$) . liftIO . hPrint lh . (<+>) "Total"
            . pretty . unCount) 0
        $ runState @Stats (flip (<$) . liftIO . hPrint lh . pretty) mempty
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

-- CuratedPassSetSpec O3 doesn't apply -inline even though opt -O3 does.
passes' :: LLVM.Pass.PassSetSpec
passes' = LLVM.Pass.PassSetSpec
    { LLVM.Pass.transforms =
        [ LLVM.Opt.PartialInlining
        , LLVM.Opt.FunctionInlining 225
        ]
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
    , Has (State INetPairs) sig m, Has (State INetSize) sig m
    , Has (State Stats) sig m)
    => Algebra (TraceRewrite :+: sig) (TraceRewriteC m)
  where
    alg hdl sig ctx = TraceRewriteC $ \h -> case sig of
        L (TraceRewrite _ _ n0 n1 cont) -> do
            size0 <- gets unINetSize
            r <- runTraceRewrite h . hdl $ cont <$ ctx
            lint size0 n0 n1
            let nh0 = nodeHead n0
                nh1 = nodeHead n1
                statKey = if nh0 < nh1 then (nh0, nh1) else (nh1, nh0)
            modify $ _Count %~ succ
            modify $ _Stats . at statKey %~ Just . maybe 1 succ
            count <- gets unCount
            netSize <- gets (IM.size . unINet)
            pairsSize <- gets (IS.size . unINetPairs)
            case (n0, n1) of
                -- These nodes are extremely abundant and usually uninteresting.
                (LamNode {}, _) -> pure ()
                (_, LamNode {}) -> pure ()
                (AppNode {}, DupNode {}) -> pure ()
                (DupNode {}, AppNode {}) -> pure ()
                (DupNode {}, DupNode {}) -> pure ()
                (DeadNode {}, _) -> pure ()
                (_, DeadNode {}) -> pure ()
                (BoxNode {}, _) -> pure ()
                (_, BoxNode {}) -> pure ()
                (TBuildNode {}, TSplitNode {}) -> pure ()
                (TSplitNode {}, TBuildNode {}) -> pure ()
                (TCloseNode {}, _) -> pure ()
                (_, TCloseNode {}) -> pure ()
                _ -> liftIO $ hPrint h
                    $ pretty count
                    <+> pretty netSize
                    <+> pretty pairsSize
                    <+> pretty size0
                    <> nest 4 (line <> pretty n0 <> line <> pretty n1)
            when (count > 1000000) $ do
                stats <- get @Stats
                liftIO $ hPrint h $ pretty stats
                error "too much work, giving up"
            pure r
        R other -> alg (runTraceRewrite h . hdl) other ctx
      where
        lint :: HasRewriter sig m => Int -> INetF Ref -> INetF Ref -> m ()
        lint size n0 n1 = do
            net <- gets unINet
            (traverse_ . traverse) lintRef . snd $ IM.split (pred size) net
            let f :: Fold (INetF Ref) Ref
                f = traverse . to ((net IM.!?) . refNode) . traverse . traverse
            traverse_ lintRef $ n0 ^.. f
            traverse_ lintRef $ n1 ^.. f
          where
            lintRef :: HasRewriter sig m => Ref -> m ()
            lintRef (Ref (-1) (-1)) = abort "uninitialised ref"
            lintRef r3 = do
                net <- get @INet
                case net ^? ix (refNode r3) of
                    Nothing -> abort $ "missing node:" <+> pretty r3
                    Just n4 -> case n4 ^? ix (refPort r3) of
                        Nothing -> abort $ "missing port:" <+> pretty r3
                        Just _ -> pure ()

            abort :: HasRewriter sig m => Doc ann -> m a
            abort msg = do
                deleteBoxes
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

newtype Stats = Stats { unStats :: M.Map (NodeHead, NodeHead) Int }
    deriving newtype (Eq, Ord, Monoid, Semigroup)

instance Pretty Stats where
    pretty
        = vcat . (uncurry ((. pretty) . (<+>)
            . uncurry ((. pretty) . (<+>) . pretty)) <$>)
        . M.assocs . unStats

_Stats :: Iso' Stats (M.Map (NodeHead, NodeHead) Int)
_Stats = iso unStats Stats
{-# INLINE _Stats #-}

data NodeHead
    = AppHead | LamHead | DupHead | DeadHead | BoxHead
    | ExternalRootHead | PrivateRootHead | AccumIOHead | AccumNBHead
    | OperandHead | OperandAHead | OperandPHead
    | IOHead | IOAHead | IOPHead | IOPureHead | IOContHead
    | ReturnCHead | ReturnFHead
    | Bind0BHead | Bind0CHead | Bind0FHead | Bind1CHead | Bind1FHead
    | Branch0CHead | Branch0FHead
    | LabelHead | NamedBlockHead | Merge0Head | Merge1Head
    | TBuildHead | TEntryHead | TSplitHead
    | TCloseHead | TLeaveHead | TMatchHead
    | PArgumentHead | PReduceHead
    deriving stock (Eq, Ord)

instance Pretty NodeHead where
    pretty AppHead = "App"
    pretty LamHead = "Lam"
    pretty DupHead = "Dup"
    pretty DeadHead = "Dead"
    pretty ExternalRootHead = "ExternalRoot"
    pretty PrivateRootHead = "PrivateRoot"
    pretty AccumIOHead = "AccumIO"
    pretty AccumNBHead = "AccumNB"
    pretty BoxHead = "Box"
    pretty OperandHead = "Operand"
    pretty OperandAHead = "OperandA"
    pretty OperandPHead = "OperandP"
    pretty IOHead = "IO"
    pretty IOAHead = "IOA"
    pretty IOPHead = "IOP"
    pretty IOPureHead = "IOPure"
    pretty IOContHead = "IOCont"
    pretty ReturnCHead = "ReturnC"
    pretty ReturnFHead = "ReturnF"
    pretty Bind0BHead = "Bind0B"
    pretty Bind0CHead = "Bind0C"
    pretty Bind0FHead = "Bind0F"
    pretty Bind1CHead = "Bind1C"
    pretty Bind1FHead = "Bind1F"
    pretty Branch0CHead = "Branch0C"
    pretty Branch0FHead = "Branch0F"
    pretty LabelHead = "Label"
    pretty NamedBlockHead = "NamedBlock"
    pretty Merge0Head = "Merge0"
    pretty Merge1Head = "Merge1"
    pretty TBuildHead = "TBuild"
    pretty TEntryHead = "TEntry"
    pretty TSplitHead = "TSplit"
    pretty TCloseHead = "TClose"
    pretty TLeaveHead = "TLeave"
    pretty TMatchHead = "TMatch"
    pretty PArgumentHead = "PArgument"
    pretty PReduceHead = "PReduce"

nodeHead :: INetF a -> NodeHead
nodeHead x = case x of
    AppNode {} -> AppHead
    LamNode {} -> LamHead
    DupNode {} -> DupHead
    DeadNode {} -> DeadHead
    BoxNode {} -> BoxHead
    ExternalRootNode {} -> ExternalRootHead
    PrivateRootNode {} -> PrivateRootHead
    AccumIONode {} -> AccumIOHead
    AccumNBNode {} -> AccumNBHead
    OperandNode {} -> OperandHead
    OperandANode {} -> OperandAHead
    OperandPNode {} -> OperandPHead
    IONode {} -> IOHead
    IOANode {} -> IOAHead
    IOPNode {} -> IOPHead
    IOPureNode {} -> IOPureHead
    IOContNode {} -> IOContHead
    ReturnCNode {} -> ReturnCHead
    ReturnFNode {} -> ReturnFHead
    Bind0BNode {} -> Bind0BHead
    Bind0CNode {} -> Bind0CHead
    Bind0FNode {} -> Bind0FHead
    Bind1CNode {} -> Bind1CHead
    Bind1FNode {} -> Bind1FHead
    Branch0CNode {} -> Branch0CHead
    Branch0FNode {} -> Branch0FHead
    LabelNode {} -> LabelHead
    NamedBlockNode {} -> NamedBlockHead
    Merge0Node {} -> Merge0Head
    Merge1Node {} -> Merge1Head
    TBuildNode {} -> TBuildHead
    TEntryNode {} -> TEntryHead
    TSplitNode {} -> TSplitHead
    TCloseNode {} -> TCloseHead
    TLeaveNode {} -> TLeaveHead
    TMatchNode {} -> TMatchHead
    PArgumentNode {} -> PArgumentHead
    PReduceNode {} -> PReduceHead

hPutDoc :: Handle -> Doc ann -> IO ()
hPutDoc h doc = renderIO h $ layoutPretty opts doc
  where
    opts = defaultLayoutOptions
        { layoutPageWidth = Unbounded
        }

