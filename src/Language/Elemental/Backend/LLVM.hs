{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Language.Elemental.Backend.LLVM
    ( compileProgram
    , compileExternal
    , compileFunction
    , compileBlockList
    , compileBlock
    , compileInstruction
    , compileTerminator
    , compileOperand
    , toLlvmName
    , toLlvmType
    , toLlvmInt
    , toLlvmNat
    , LlvmOp
    , orUndef
    , Scope
    ) where

import Control.Algebra ((:+:))
import Control.Carrier.Reader (Reader, asks, local, runReader)
import Control.Carrier.State.Church (State, evalState, get, modify)
import Control.Lens hiding (Empty, op)
import Data.Foldable (fold, foldl', foldrM)
import Data.Functor (void)
import Data.Graph (SCC(AcyclicSCC, CyclicSCC), stronglyConnComp)
import Data.IntMap qualified as IM
import Data.List (elemIndex, nub)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import LLVM.AST qualified as LLVM
import LLVM.AST.CallingConvention qualified as LLVM.CConv
import LLVM.AST.Constant qualified as LLVM.Constant
import LLVM.AST.Linkage qualified as LLVM.Linkage
import LLVM.AST.Type qualified as LLVM.Type
import Math.NumberTheory.Logarithms (naturalLog2)
import Numeric.Natural (Natural)
import Prettyprinter (pretty, (<+>))

import Control.Carrier.IRBuilder
import Control.Carrier.ModuleBuilder
import Language.Elemental.Backend

type LlvmOp = (LLVM.Type, Maybe LLVM.Operand)

type Scope
    = Reader (M.Map FunctionName LLVM.Operand)
    :+: Reader (M.Map Name Function)
    :+: Reader (IM.IntMap LLVM.Name)

-- | Assumes that functions are defined before the functions that use them.
compileProgram :: Program -> [LLVM.Definition]
compileProgram (Program exts funcs) = run
    $ runModuleBuilder (const . pure) emptyModuleBuilder
    $ runReader initScope $ runReader pfuncs $ runReader (IM.empty @LLVM.Name)
    $ foldr compileExternal (foldr compileFunction (pure ()) efuncs) exts
  where
    extMap = M.fromList $ (\(name := ext) -> (name, ext)) <$> exts

    initScope :: M.Map FunctionName LLVM.Operand
    efuncs :: [ForeignNamed (LLVM.Linkage.Linkage, Function)]
    pfuncs :: M.Map Name Function
    (initScope, efuncs, pfuncs) = run $ evalState @Int 0
        $ fmap fold $ traverse nameFunction $ M.assocs
        $ foldl' (flip toExplicit) M.empty $ stronglyConnComp $ toConn <$> funcs

    nameFunction
        :: forall sig m. Has (State Int) sig m
        => (FunctionName, Function)
        -> m (M.Map FunctionName LLVM.Operand
            , [ForeignNamed (LLVM.Linkage.Linkage, Function)]
            , M.Map Name Function)
    nameFunction (funcName@(PrivateName name), func) = go
      where
        go :: m (M.Map FunctionName LLVM.Operand
            , [ForeignNamed (LLVM.Linkage.Linkage, Function)]
            , M.Map Name Function)
        go = do
            idx <- get @Int <* modify @Int succ
            let namef = ForeignName . ("__elem_" <>) . fromString $ show idx
                opf = LLVM.ConstantOperand $ LLVM.Constant.GlobalReference t
                    $ toLlvmName namef
                t = LLVM.Type.ptr $ LLVM.FunctionType tret targs False
                targs = toLlvmType . (\(_ := t') -> t') <$> functionArgs func
                tret = toLlvmType $ functionRet func
                lf = (LLVM.Linkage.Private, func)
                pf = M.singleton name func
            if M.member namef extMap
                then go
                else pure (M.singleton funcName opf, [namef := lf], pf)
    nameFunction (ExternalName name, func)
        = pure (mempty, [name := (LLVM.Linkage.External, func)], mempty)

    toExplicit
        :: SCC NamedFunction
        -> M.Map FunctionName Function -> M.Map FunctionName Function
    toExplicit scc fs = fs <> case scc of
        AcyclicSCC nf -> either go sing nf
        CyclicSCC nfs -> foldMap (either go sing) nfs
      where
        iargs = scc ^.. traverse . implicitArgs
        trets = scc ^.. traverse . retTypes

        go :: Named ImplicitFunction -> M.Map FunctionName Function
        go (name := ImplicitFunction args' bs)
            = M.singleton (PrivateName name) (Function (nub args) ret bs)
          where
            args = args' <> iargs
            ret = case nub trets of
                [] -> IntType 0
                [t] -> t
                ts -> TupleType ts

        sing (name := func) = M.singleton name func

        implicitArgs :: Monoid a => Getting a NamedFunction (Named Type)
        implicitArgs = _Left . traverse . _ifunctionBlocks . (blockListBlocks
            . _blockTerm . _TailCall . _1 . to ((fs M.!?) . PrivateName)
            . traverse . _functionArgs . dropping 1 traverse
            <> blockListFreeRefs . to (uncurry $ flip (:=)))

        retTypes :: Monoid a => Getting a NamedFunction Type
        retTypes = _Left . traverse . _ifunctionBlocks . (blRets <> blTails)

        blRets :: Fold BlockList Type
        blRets = blockListBlocks . _blockTerm . _Return . to opType

        blTails :: Fold BlockList Type
        blTails = blockListBlocks . _blockTerm . _TailCall . _1
            . to ((fs M.!?) . PrivateName) . traverse . _functionRet

    toConn :: NamedFunction -> (NamedFunction, FunctionName, [FunctionName])
    toConn nf = (nf, name, nf ^.. refs)
      where
        name = case nf of
            Left (name' := _) -> PrivateName name'
            Right (name' := _) -> name'

        refs :: Fold NamedFunction FunctionName
        refs = _Left . traverse . _ifunctionBlocks . blockListBlocks
            . _blockTerm . _TailCall . _1 . to PrivateName

compileExternal
    :: (Has ModuleBuilder sig m, Has Scope sig m)
    => ForeignNamed External -> m r -> m r
compileExternal (name := External targs tret) cont = do
    let lname = toLlvmName name
    lopf <- extern lname (toLlvmType <$> targs) (toLlvmType tret)
    local (M.insert (ExternalName name) lopf) cont

compileFunction
    :: forall sig m r. (Has ModuleBuilder sig m, Has Scope sig m)
    => ForeignNamed (LLVM.Linkage.Linkage, Function) -> m r -> m r
compileFunction (name := (linkage, Function args ret bs)) cont = do
    let tret = toLlvmType ret
        lname = toLlvmName name
    _ <- function lname (toLlvmType . namedValue <$> args) tret linkage
        $ \lops -> do
            foldr bindOp (compileBlockList ret bs) $ zip args lops
            void block
    cont
  where
    namedValue :: Named a -> a
    namedValue (_ := a) = a

    bindOp :: (Named Type, LLVM.Operand) -> IRBuilderC m r' -> IRBuilderC m r'
    bindOp (name' := _, lop) = local (M.insert (PrivateName name') lop)

compileBlockList
    :: (Has IRBuilder sig m, Has Scope sig m) => Type -> BlockList -> m ()
compileBlockList tret (BlockList be (NamedBlockList bs)) = do
    labels <- traverse (const fresh) bs
    local (labels <>) $ compileBlock tret be $ foldr go (pure ()) (IM.assocs bs)
  where
    go (idx, b) cont = do
        llbl <- asks $ fromMaybe abort . (IM.!? idx)
        emitBlockStart llbl
        compileBlock tret b cont
      where
        abort = error . show $ "compileBlockList: label not in scope:"
            <+> pretty (Label idx)

compileBlock
    :: forall sig m. (Has IRBuilder sig m, Has Scope sig m)
    => Type -> Block -> m () -> m ()
compileBlock tret (Block instrs term) cont
    = foldr go (compileTerminator tret term *> cont) instrs
  where
    go :: Named Instruction -> m r -> m r
    go (name := instr) cont' = do
        lop <- compileInstruction instr
        local (M.insert (PrivateName name) $ orUndef lop) cont'

compileInstruction
    :: (Has IRBuilder sig m, Has Scope sig m) => Instruction -> m LlvmOp
compileInstruction = \case
    Pure op -> compileOperand op
    Call t name args -> do
        lopf <- fromMaybe (error $ "function not in scope: " <> show name)
            <$> asks (M.!? name)
        largs <- traverse ((mkParam . orUndef <$>) . compileOperand) args
        let call = LLVM.Call Nothing LLVM.CConv.C [] (Right lopf) largs [] []
            ltret = toLlvmType t
        (,) ltret <$> case t of
            IntType 0 -> Nothing <$ emitInstrVoid call
            _ -> Just <$> emitInstr ltret call
    Load ptr -> do
        (lt, mlptr) <- compileOperand ptr
        let lptr = fromMaybe (undef $ LLVM.Type.ptr lt) mlptr
        ((,) lt . Just <$>) $ emitInstr lt $ LLVM.Load True lptr Nothing 1 []
    Store ptr op -> do
        (ltp, mlptr) <- compileOperand ptr
        (lt, mlop) <- compileOperand op
        let lptr = fromMaybe (undef $ LLVM.Type.ptr ltp) mlptr
            lop = fromMaybe (undef lt) mlop
        emitInstrVoid $ LLVM.Store True lptr lop Nothing 1 []
        pure (LLVM.VoidType, Nothing)
  where
    mkParam a = (a, [])

compileTerminator
    :: (Has IRBuilder sig m, Has Scope sig m) => Type -> Terminator -> m ()
compileTerminator tret = \case
    Jump lbl -> do
        llbl <- getLabel lbl
        emitTerm $ LLVM.Br llbl []
    Branch opc lblt lblf -> do
        lopc <- orUndef <$> compileOperand opc
        llblt <- getLabel lblt
        llblf <- getLabel lblf
        emitTerm $ LLVM.CondBr lopc llblt llblf []
    Return op
        | opType op == tret -> do
            (_, lop) <- compileOperand op
            emitTerm $ LLVM.Ret lop []
        | otherwise -> do
            let opt = Tuple $ ix idx .~ op $ (`Reference` Name 0) <$> ts
                idx = fromMaybe abortRetType $ elemIndex (opType op) ts
                ts = case tret of
                    TupleType ts' -> ts'
                    _ -> abortRetType
            (_, lop) <- compileOperand opt
            emitTerm $ LLVM.Ret lop []
    TailCall name parg -> do
        let abort = error . show
                $ "compileTerminator: function not in scope:" <+> pretty name
            mkArg (name' := t) = Reference t name'
        Function args ret _ <- asks $ fromMaybe abort . (M.!? name)
        let args' = zipWith (fromMaybe . mkArg) args
                $ Just parg : repeat Nothing
        (_, lop) <- compileInstruction $ Call ret (PrivateName name) args'
        if ret == tret then emitTerm $ LLVM.Ret lop [] else do
            let idx = fromMaybe abortRetType $ elemIndex tret ts
                ts = case ret of
                    TupleType ts' -> ts'
                    _ -> abortRetType
                op = fromMaybe abortRetType lop
                tmp = Name $ -2
            (_, lop') <- local (M.insert (PrivateName tmp) op)
                $ compileOperand $ GetElement idx $ Reference ret tmp
            emitTerm $ LLVM.Ret lop' []
    Unreachable -> emitTerm $ LLVM.Unreachable []
  where
    abortRetType = error . show
        $ "compileTerminator: incompatible return type" <+> pretty tret
    getLabel lbl = asks $ fromMaybe abort . (IM.!? unLabel lbl)
      where
        abort = error . show
            $ "compileTerminator: label not in scope:" <+> pretty lbl

compileOperand :: (Has IRBuilder sig m, Has Scope sig m) => Operand -> m LlvmOp
compileOperand = skipVoid $ \case
    Reference t name -> asks $ (,) (toLlvmType t) . (M.!? PrivateName name)
    Address t addr -> pure $ (,) (toLlvmType t) $ Just $ LLVM.ConstantOperand
        $ LLVM.Constant.IntToPtr (toLlvmNat addr) $ LLVM.Type.ptr $ toLlvmType t
    Empty -> pure (LLVM.VoidType, Nothing)
    Constant bit -> pure $ (,) LLVM.Type.i1 $ Just
        $ LLVM.ConstantOperand $ LLVM.Constant.Int 1 $ toInteger $ fromEnum bit
    IsolateBit size idx op -> do
        lop <- orUndef <$> compileOperand op
        lops <- emitInstr (LLVM.IntegerType $ succ $ fromIntegral size)
            $ LLVM.LShr False lop (toLlvmInt size $ toInteger idx) []
        ((,) LLVM.Type.i1 . Just <$>) $ emitInstr LLVM.Type.i1
            $ LLVM.Trunc lops LLVM.Type.i1 []
    InsertBit size op1 op2 -> case size of
        0 -> compileOperand op1
        _ -> do
            lop1 <- orUndef <$> compileOperand op1
            lop2 <- orUndef <$> compileOperand op2
            let lt = LLVM.IntegerType $ succ $ fromIntegral size
                sh = toInteger size
            lopz1 <- emitInstr lt $ LLVM.ZExt lop1 lt []
            lopz2 <- emitInstr lt $ LLVM.ZExt lop2 lt []
            lops1 <- emitInstr lt
                $ LLVM.Shl False True lopz1 (toLlvmInt (succ size) sh) []
            ((,) lt . Just <$>) $ emitInstr lt $ LLVM.Or lops1 lopz2 []
    Select opc opt opf -> do
        (ltc, mlopc) <- compileOperand opc
        (ltt, mlopt) <- compileOperand opt
        (ltf, mlopf) <- compileOperand opf
        let lopc = fromMaybe (undef ltc) mlopc
            lopt = fromMaybe (undef ltt) mlopt
            lopf = fromMaybe (undef ltf) mlopf
        ((,) ltt . Just <$>) $ emitInstr ltt $ LLVM.Select lopc lopt lopf []
    Tuple ops -> do
        lops <- traverse (fmap orUndef . compileOperand) ops
        let lt = toLlvmType $ TupleType $ opType <$> ops
            lopz = LLVM.ConstantOperand $ LLVM.Constant.AggregateZero lt
        ((,) lt . Just <$>) $ foldrM (insertStruct lt) lopz $ zip [0..] lops
    opSelf@(GetElement idx op) -> do
        lop <- orUndef <$> compileOperand op
        let lt = toLlvmType $ opType opSelf
        ((,) lt . Just <$>) $ emitInstr lt
            $ LLVM.ExtractValue lop [fromIntegral idx] []
  where
    insertStruct
        :: Has IRBuilder sig m
        => LLVM.Type -> (Int, LLVM.Operand) -> LLVM.Operand -> m LLVM.Operand
    insertStruct lt (idx, lop1) lops = emitInstr lt
        $ LLVM.InsertValue lops lop1 [fromIntegral idx] []

    skipVoid
        :: Has IRBuilder sig m => (Operand -> m LlvmOp) -> Operand -> m LlvmOp
    skipVoid cont op = case opType op of
        IntType 0 -> pure (LLVM.VoidType, Nothing)
        _ -> cont op

undef :: LLVM.Type -> LLVM.Operand
undef = LLVM.ConstantOperand . LLVM.Constant.Undef

orUndef :: LlvmOp -> LLVM.Operand
orUndef (lt, mlop) = fromMaybe (undef lt) mlop

toLlvmName :: ForeignName -> LLVM.Name
toLlvmName (ForeignName s) = LLVM.Name s

toLlvmType :: Type -> LLVM.Type
toLlvmType = \case
    IntType 0 -> LLVM.VoidType
    IntType size -> LLVM.IntegerType $ fromIntegral size
    TupleType ts -> LLVM.StructureType False $ toLlvmType <$> ts

toLlvmInt :: Integral n => n -> Integer -> LLVM.Operand
toLlvmInt size n
    = LLVM.ConstantOperand $ LLVM.Constant.Int (fromIntegral size) n

toLlvmNat :: Natural -> LLVM.Constant.Constant
toLlvmNat n = LLVM.Constant.Int size $ toInteger n
  where
    size :: Num n => n
    size
        | n == 0 = 1
        | otherwise = fromIntegral $ naturalLog2 n + 1

