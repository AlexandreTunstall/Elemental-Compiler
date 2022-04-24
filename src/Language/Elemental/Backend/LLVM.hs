{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Elemental.Backend.LLVM
    ( compileProgram
    , compileExternal
    , compileFunction
    , compileBody
    , compileInstruction
    , compileOperand
    , toLlvmName
    , toLlvmType
    , toLlvmInt
    , toLlvmNat
    , LlvmOp
    , orUndef
    , Scope
    ) where

import Control.Carrier.Reader (Reader, asks, local, runReader)
import Data.Foldable (foldrM)
import Data.Functor (void)
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

import Control.Carrier.IRBuilder
import Control.Carrier.ModuleBuilder
import Language.Elemental.Backend

type LlvmOp = (LLVM.Type, Maybe LLVM.Operand)

type Scope = Reader (M.Map Name LLVM.Operand)

-- | Assumes that functions are defined before the functions that use them.
compileProgram :: Program -> [LLVM.Definition]
compileProgram (Program exts funcs) = run
    $ runModuleBuilder (const . pure) emptyModuleBuilder
    $ runReader @(M.Map Name LLVM.Operand) M.empty
    $ foldr compileExternal (foldr compileFunction (pure ()) funcs) exts

compileExternal
    :: (Has ModuleBuilder sig m, Has Scope sig m)
    => Named External -> m r -> m r
compileExternal (name := External targs tret) cont = do
    let (_, lname) = toLlvmName name
    lopf <- extern lname (toLlvmType <$> targs) (toLlvmType tret)
    local (M.insert name lopf) cont

compileFunction
    :: forall sig m r. (Has ModuleBuilder sig m, Has Scope sig m)
    => Named Function -> m r -> m r
compileFunction (name := Function args b) cont = do
    let tret = toLlvmType $ instrType $ bodyTerm b
        (linkage, lname) = toLlvmName name
    lopf <- function lname (toLlvmType . namedValue <$> args) tret linkage
        $ \lops -> do
            foldr bindOp (compileBody b >>= mkRet) $ zip args lops
            void block
    local (M.insert name lopf) cont
  where
    namedValue :: Named a -> a
    namedValue (_ := a) = a

    bindOp :: (Named Type, LLVM.Operand) -> IRBuilderC m r' -> IRBuilderC m r'
    bindOp (name' := _, lop) = local (M.insert name' lop)

    mkRet :: LlvmOp -> IRBuilderC m ()
    mkRet = emitTerm . ($ []) . LLVM.Ret . snd

compileBody
    :: forall sig m. (Has IRBuilder sig m, Has Scope sig m) => Body -> m LlvmOp
compileBody (Body instrs term) = foldr go (compileInstruction term) instrs
  where
    go :: Named Instruction -> m r -> m r
    go (name := instr) cont = do
        lop <- compileInstruction instr
        case name of
            UnusedName -> cont
            _ -> local (M.insert name $ orUndef lop) cont

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
    Branch opc bt bf -> do
        lopc <- orUndef <$> compileOperand opc
        lbt <- fresh
        lbf <- fresh
        lbr <- fresh
        emitTerm $ LLVM.CondBr lopc lbt lbf []
        emitBlockStart lbt
        (ltt, mlopt) <- compileBody bt
        lbt' <- currentBlock
        emitTerm $ LLVM.Br lbr []
        emitBlockStart lbf
        (ltf, mlopf) <- compileBody bf
        lbf' <- currentBlock
        emitTerm $ LLVM.Br lbr []
        emitBlockStart lbr
        case (mlopt, mlopf) of
            (Nothing, Nothing) -> pure (ltt, Nothing)
            _ -> do
                let lopt = orUndef (ltt, mlopt)
                    lopf = orUndef (ltf, mlopf)
                ((,) ltt . Just <$>) $ emitInstr ltt
                    $ LLVM.Phi ltt [(lopt, lbt'), (lopf, lbf')] []
  where
    mkParam a = (a, [])

compileOperand :: (Has IRBuilder sig m, Has Scope sig m) => Operand -> m LlvmOp
compileOperand = skipVoid $ \case
    Reference t name -> asks $ (,) (toLlvmType t) . (M.!? name)
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

toLlvmName :: Name -> (LLVM.Linkage.Linkage, LLVM.Name)
toLlvmName name = case name of
    Name {} -> (LLVM.Linkage.Private, LLVM.Name $ go name)
    ExternalName {} -> (LLVM.Linkage.External, LLVM.Name $ go name)
    SubName {} -> (LLVM.Linkage.Private, LLVM.Name $ go name)
    UnusedName {} -> (LLVM.Linkage.Private, LLVM.Name $ go name)
  where
    go = \case
        Name idx -> fromString $ show idx
        ExternalName name' -> name'
        SubName name' idx -> go name' <> fromString ('.' : show idx)
        UnusedName -> "_"

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

