{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

-- | Functions to convert an Elemental program into an LLVM module.
module Language.Elemental.Emit
    ( emitProgram
    , emitDeclScope
    , emitDecl
    , emitExpr
    , emitExprIO
    , emitExprOp
    , foldArrow
    , foldForall
    , foldIO
    , foldPointer
    , llvmType
    , llvmForeignName
    , llvmAddress
    ) where

import Control.Monad (void)
import Data.Text.Short (toShortByteString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Void (absurd)
import LLVM.AST qualified as LLVM
import LLVM.AST.CallingConvention qualified as LLVM.CallConv
import LLVM.AST.Constant qualified as LLVM.Constant
import LLVM.AST.Type qualified as LLVM.Type
import Math.NumberTheory.Logarithms (naturalLog2)

import Control.Carrier.ModuleBuilder
import Control.Effect.IRBuilder
import Language.Elemental.AST.Decl
import Language.Elemental.AST.Expr
import Language.Elemental.AST.Program
import Language.Elemental.AST.Type
import Language.Elemental.Primitive
import Language.Elemental.Singleton


-- | Emits a program as a list of LLVM definitions.
emitProgram :: Program -> [LLVM.Definition]
emitProgram (Program decls)
    = runModuleBuilder const emptyModuleBuilder $ emitDeclScope decls

-- | Emits a list of declarations.
emitDeclScope :: Has ModuleBuilder sig m => DeclScope '[] rest -> m ()
emitDeclScope = \case
    DeclNil -> pure ()
    DeclCons decl decls -> do
        expr <- emitDecl decl
        case declType SNil decl of
            SNothing -> emitDeclScope decls
            SJust t -> emitDeclScope
                $ substituteDeclScope (t :^ SNil) SZero expr decls

{-|
    Emits a declaration, returning the expression to add to the scope if the
    declaration adds anything to the scope.
-}
emitDecl
    :: Has ModuleBuilder sig m
    => Decl '[] mt -> m (FoldMaybe () (Expr 'Zero '[]) mt)
emitDecl = \case
    Binding expr -> pure expr
    ForeignImport fname t -> do
        let ltargs = sForeignArgs t
            ltargs' = llvmArgs ltargs
            ltret = sForeignRet t
            ltret' = llvmType ltret
        op <- extern (llvmForeignName fname) ltargs' ltret'
        pure $ wrapImport SZero SNil t $ Call (Right op) [] ltargs ltret
    ForeignExport fname expr -> do
        let t = exprType SZero SNil expr
            ltargs = sForeignArgs t
            ltargs' = llvmArgs ltargs
            ltret = sForeignRet t
            ltret' = llvmType ltret
            export ops = applyArgs ltret ltargs ops
                $ wrapExport SZero SNil t expr
        _ <- function (llvmForeignName fname) ltargs' ltret' $ emitExpr . export
        pure ()
    ForeignPrimitive pfin -> pure $ primitiveExprs !!^ pfin
    ForeignAddress addr pk t -> pure $ Addr addr pk t
  where
    llvmArgs :: SList SLlvmType lts -> [LLVM.Type]
    llvmArgs SNil = []
    llvmArgs (lt :^ lts) = llvmType lt : llvmArgs lts

    applyArgs
        :: AllIsOpType ltargs ~ 'True
        => proxy ltret -> SList SLlvmType ltargs -> [LLVM.Operand]
        -> Expr 'Zero '[] (BuildForeignType ltargs ltret)
        -> Expr 'Zero '[] ('IOType ('LlvmType ltret))
    applyArgs _ SNil [] expr = expr
    applyArgs t (ltarg :^ ltargs) (op : ops) expr
        = withAllIsOpTypeProof ltarg ltargs
        $ applyArgs t ltargs ops
        $ expr :$ LlvmOperand ltarg op
    applyArgs _ SNil ops _ = error $ "emitDecl: "
        <> show (length ops) <> " excess operands"
    applyArgs _ ltargs [] _ = error $ "emitDecl: "
        <> show (toNatural $ sLength ltargs) <> " missing operands"

-- | Emits an expression. This emits a @ret@ instruction.
emitExpr
    :: Has IRBuilder sig m => Expr 'Zero '[] ('IOType ('LlvmType lt)) -> m ()
emitExpr expr = do
    let SIOType (SLlvmType lt) = exprType SZero SNil expr
    mop <- toMaybeOp lt <$> emitExprIO expr
    emitTerm $ LLVM.Ret mop []
    void block
  where
    toMaybeOp :: SLlvmType lt -> LlvmOperandType lt -> Maybe LLVM.Operand
    toMaybeOp lt op = case sIsOpType lt of
        SFalse -> Nothing
        STrue -> Just op

{-|
    Emits an expression. Unlike 'emitExpr', this does not emit a @ret@
    instruction but instead returns the final operand for the caller to use.
-}
emitExprIO
    :: forall lt sig m. Has IRBuilder sig m
    => Expr 'Zero '[] ('IOType ('LlvmType lt)) -> m (LlvmOperandType lt)
emitExprIO = \case
    Var vidx -> absurd $ zeroNoLT vidx Refl
    App ef ex -> foldArrow
        (\tx -> emitExprIO . substituteExpr SZero (tx :^ SNil) SZero ex)
        (\Refl -> emitExprOp ex)
        (\_ -> \case {})
        (\Refl Refl tx ey ty -> foldIO
            (\Refl lt -> do
                op <- emitExprIO ey
                emitExprIO $ ex :$ LlvmOperand lt op)
            (\ez -> emitExprIO $ ex :$ ez)
            (\op et ef' -> withProof (subIncElim SZero SZero ty tx Refl)
                $ emitCondBr op
                    (BindIO :@ tx :$ et :@ ty :$ ex)
                    (BindIO :@ tx :$ ef' :@ ty :$ ex))
            ey)
        (\Refl Refl _ -> foldPointer
            (\tx pop -> do
                let lt = sMarshall tx
                op <- emitInstr (llvmType lt) $ LLVM.Load True pop Nothing 1 []
                emitExprOp $ marshallIn SZero SNil tx $ LlvmOperand lt op)
            ex)
        (\_ -> \case {})
        (\case {})
        (\Refl Refl fop aops (ltarg :^ SNil) ltret -> do
            aop <- withAllIsOpTypeProof ltarg SNil $ emitExprOp ex
            emitCall fop (aop : aops) ltret)
        (\_ -> \case {})
        (\_ -> \case {})
        (\_ -> \case {})
        (\case {})
        (\Refl op ey -> emitCondBr op ey ex)
        (\op ey ez -> emitCondBr op (ey :$ ex) (ez :$ ex))
        ef
    TypeApp ef tx -> foldForall
        (emitExprIO . substituteExprType SZero SNil SZero tx)
        (\case {})
        (\case {})
        (\case {})
        (\case {})
        (\case {})
        (\case {})
        (\op ey ez -> emitCondBr op (ey :@ tx) (ez :@ tx))
        ef
    LlvmIO _ mop -> mop
    Call fop aops SNil ltret -> emitCall fop aops ltret
  where
    emitCall
        :: LLVM.CallableOperand -> [LLVM.Operand]
        -> SLlvmType ltret -> m (LlvmOperandType ltret)
    emitCall fop aops ltret = case sIsOpType ltret of
        SFalse -> emitInstrVoid instr
        STrue -> emitInstr (llvmType ltret) instr
      where
        instr = LLVM.Call Nothing LLVM.CallConv.C [] fop
            ((, []) <$> reverse aops) [] []

    emitCondBr
        :: LLVM.Operand
        -> Expr 'Zero '[] ('IOType ('LlvmType lt))
        -> Expr 'Zero '[] ('IOType ('LlvmType lt))
        -> m (LlvmOperandType lt)
    emitCondBr opc et ef = do
        bt <- fresh
        bf <- fresh
        br <- fresh
        emitTerm $ LLVM.CondBr opc bt bf []
        emitBlockStart bt
        opt <- emitExprIO et
        bt' <- currentBlock
        emitTerm $ LLVM.Br br []
        emitBlockStart bf
        opf <- emitExprIO ef
        bf' <- currentBlock
        emitTerm $ LLVM.Br br []
        emitBlockStart br
        case sIsOpType lt of
            SFalse -> pure ()
            STrue -> emitInstr (llvmType lt)
                $ LLVM.Phi (llvmType lt) [(opt, bt'), (opf, bf')] []
      where
        SIOType (SLlvmType lt) = exprType SZero SNil et

{-|
    Emits a pure LLVM operand.

    This still needs an 'IRBuilder' effect because it may need to emit
    instructions for operand conversion. All instructions emitted by this
    function do not have any side effects; they may be eliminated by the LLVM
    optimiser if their result is unused.
-}
emitExprOp
    :: forall lt sig m. Has IRBuilder sig m
    => Expr 'Zero '[] ('LlvmType lt) -> m (LlvmOperandType lt)
emitExprOp = \case
    Var vidx -> absurd $ zeroNoLT vidx Refl
    App ef ex -> foldArrow
        (\tx -> emitExprOp . substituteExpr SZero (tx :^ SNil) SZero ex)
        (\case {})
        (\_ -> \case {})
        (\_ -> \case {})
        (\_ -> \case {})
        (\_ -> \case {})
        (\case {})
        (\Refl Refl _ _ (_ :^ ltargs) _ -> case ltargs of {})
        (\Refl Refl bidx size -> do
            let SLlvmType lt = exprType SZero SNil ex
                li1 = llvmType $ SLlvmInt $ SSucc SZero
            op <- withProof (ltRightPredSucc bidx size Refl)
                $ emitExprOp ex
            shifted <- emitInstr (llvmType lt) $ LLVM.LShr False op
                (LLVM.ConstantOperand $ LLVM.Constant.Int
                    (fromIntegral $ toNatural size)
                    (fromIntegral $ toNatural bidx)) []
            emitInstr li1 $ LLVM.Trunc shifted li1 [])
        (\_ -> \case {})
        (\Refl Refl size bop -> case sCmpNat size SZero of
            SLT -> absurd $ zeroNoLT size Refl
            SEQ -> pure bop
            SGT -> do
                iop <- emitExprOp ex
                let lt = SLlvmInt $ SSucc size
                    ft = llvmType lt
                    usize :: Integral n => n
                    usize = fromIntegral $ toNatural size
                bext <- emitInstr ft $ LLVM.ZExt bop ft []
                iext <- emitInstr ft $ LLVM.ZExt iop ft []
                bsh <- emitInstr ft $ LLVM.Shl False True bext
                    (LLVM.ConstantOperand $ LLVM.Constant.Int
                        (usize + 1) usize) []
                emitInstr ft $ LLVM.Or bsh iext [])
        (\case {})
        (\Refl op ey -> emitSelect op ey ex)
        (\op ey ez -> emitSelect op (ey :$ ex) (ez :$ ex))
        ef
    TypeApp ef tx -> foldForall
        (emitExprOp . substituteExprType SZero SNil SZero tx)
        (\case {})
        (\case {})
        (\case {})
        (\case {})
        (\case {})
        (\case {})
        (\op ey ez -> emitSelect op (ey :@ tx) (ez :@ tx))
        ef
    LlvmOperand _ op -> pure op
    Call _ _ ltargs _ -> case ltargs of {}
  where
    emitSelect
        :: LLVM.Operand
        -> Expr 'Zero '[] ('LlvmType lt) -> Expr 'Zero '[] ('LlvmType lt)
        -> m (LlvmOperandType lt)
    emitSelect opc et ef = case sIsOpType lt of
        SFalse -> pure ()
        STrue -> do
            opt <- emitExprOp et
            opf <- emitExprOp ef
            emitInstr (llvmType lt) $ LLVM.Select opc opt opf []
      where
        SLlvmType lt = exprType SZero SNil et

-- | Folds an arrow using the given continuations for each possible value.
foldArrow
    :: forall ta tb sig r m. Has IRBuilder sig m
    => (SType 'Zero ta -> Expr 'Zero '[ta] tb -> m r)
    -- ^ Lam
    -> (tb :~: 'IOType ta -> m r)
    -- ^ PureIO :@ ta
    -> (forall tx. ta :~: 'IOType tx -> tb :~: 'Forall ((Increment 'Zero tx
        :-> 'IOType ('TypeVar 'Zero)) :-> 'IOType ('TypeVar 'Zero))
        -> SType 'Zero tx -> m r)
    -- ^ BindIO :@ _
    -> (forall tx ty. ta :~: 'Arrow tx ('IOType ty) -> tb :~: 'IOType ty
        -> SType 'Zero tx -> Expr 'Zero '[] ('IOType tx)
        -> SType 'Zero ty -> m r)
    -- ^ BindIO :@ _ :$ _ :@ _
    -> (forall tx. ta :~: 'PointerType 'ReadPointer tx -> tb :~: 'IOType tx
        -> SType 'Zero tx -> m r)
    -- ^ LoadPointer :@ _
    -> (forall tx. ta :~: 'PointerType 'WritePointer tx
        -> tb :~: (tx :-> 'IOType UnitType) -> SType 'Zero tx -> m r)
    -- ^ StorePointer :@ _
    -> (tb :~: 'IOType UnitType
        -> Expr 'Zero '[] ('PointerType 'WritePointer ta) -> m r)
    -- ^ StorePointer :@ ta :$ _
    -> (forall ltarg ltargs ltret. AllIsOpType (ltarg ': ltargs) ~ 'True
        => ta :~: 'LlvmType ltarg
        -> tb :~: BuildForeignType ltargs ltret
        -> LLVM.CallableOperand -> [LLVM.Operand]
        -> SList SLlvmType (ltarg ': ltargs) -> SLlvmType ltret -> m r)
    -- ^ Call _ _ _ _
    -> (forall bidx size. CmpNat bidx size ~ 'LT
        => ta :~: 'LlvmType ('LlvmInt size)
        -> tb :~: 'LlvmType ('LlvmInt ('Succ 'Zero))
        -> SNat bidx -> SNat size -> m r)
    -- ^ IsolateBit _
    -> (forall size. ta :~: 'LlvmType ('LlvmInt ('Succ 'Zero))
        -> tb :~: ('LlvmType ('LlvmInt size)
            :-> 'LlvmType ('LlvmInt ('Succ size)))
        -> SNat size -> m r)
    -- ^ InsertBit _
    -> (forall size. ta :~: 'LlvmType ('LlvmInt size)
        -> tb :~: 'LlvmType ('LlvmInt ('Succ size))
        -> SNat size -> LLVM.Operand -> m r)
    -- ^ InsertBit _ :$ _
    -> (tb :~: 'Arrow ta ta -> LLVM.Operand -> m r)
    -- ^ TestBit _ :@ ta
    -> (tb :~: ta -> LLVM.Operand -> Expr 'Zero '[] ta -> m r)
    -- ^ TestBit _ :@ ta :$ _
    -> (LLVM.Operand -> Expr 'Zero '[] (ta :-> tb)
        -> Expr 'Zero '[] (ta :-> tb) -> m r)
    -- ^ TestBit _ :@ ta :-> tb :$ _ :$ _
    -> Expr 'Zero '[] (ta :-> tb) -> m r
foldArrow lam pureIO1 bindIO1 bindIO3 loadPointer1 storePointer1 storePointer2
    call isolateBit insertBit insertBit1 testBit1 testBit2 testBit3 = \case
    Var vidx -> absurd $ zeroNoLT vidx Refl
    App ef ex -> foldArrow
        (\tx -> foldArrow lam pureIO1 bindIO1 bindIO3 loadPointer1 storePointer1
                storePointer2 call isolateBit insertBit insertBit1 testBit1
                testBit2 testBit3
            . substituteExpr SZero (tx :^ SNil) SZero ex)
        (\case {})
        (\_ -> \case {})
        (\_ -> \case {})
        (\_ -> \case {})
        (\Refl Refl _ -> storePointer2 Refl ex)
        (\case {})
        (\Refl Refl fop aops (ltarg :^ ltargs) ltret -> case ltargs of
            _ :^ _ -> withAllIsOpTypeProof ltarg ltargs $ do
                aop <- emitExprOp ex
                call Refl Refl fop (aop : aops) ltargs ltret)
        (\_ -> \case {})
        (\Refl Refl size -> emitExprOp ex >>= insertBit1 Refl Refl size)
        (\_ -> \case {})
        (\Refl op -> testBit2 Refl op ex)
        (\Refl op ey -> testBit3 op ey ex)
        (\op ey ez -> testBit3 op (ey :$ ex) (ez :$ ex))
        ef
    TypeApp ef tx -> foldForall
        (foldArrow lam pureIO1 bindIO1 bindIO3 loadPointer1 storePointer1
                storePointer2 call isolateBit insertBit insertBit1 testBit1
                testBit2 testBit3
            . substituteExprType SZero SNil SZero tx)
        (\Refl -> pureIO1 Refl)
        (\Refl -> bindIO1 Refl Refl tx)
        (\Refl ty ex -> withProof (subIncElim SZero SZero tx ty Refl)
            $ bindIO3 Refl Refl ty ex tx)
        (\Refl -> loadPointer1 Refl Refl tx)
        (\Refl -> storePointer1 Refl Refl tx)
        (\Refl -> testBit1 Refl)
        (\op ey ez -> testBit3 op (ey :@ tx) (ez :@ tx))
        ef
    Lam tx ex -> lam tx ex
    Call fop aops ltargs@(_ :^ _) ltret -> call Refl Refl fop aops ltargs ltret
    IsolateBit bidx size -> isolateBit Refl Refl bidx size
    InsertBit size -> insertBit Refl Refl size

-- | Folds a forall using the given continuations for each possible value.
foldForall
    :: forall t sig r m. Has IRBuilder sig m
    => (Expr ('Succ 'Zero) '[] t -> m r)
    -- ^ TypeLam
    -> (t :~: ('TypeVar 'Zero :-> 'IOType ('TypeVar 'Zero)) -> m r)
    -- ^ PureIO
    -> (t :~: ('IOType ('TypeVar 'Zero) :-> 'Forall (('TypeVar ('Succ 'Zero)
        :-> 'IOType ('TypeVar 'Zero)) :-> 'IOType ('TypeVar 'Zero))) -> m r)
    -- ^ BindIO
    -> (forall tx. t :~: ((Increment 'Zero tx :-> 'IOType ('TypeVar 'Zero))
        :-> 'IOType ('TypeVar 'Zero))
        -> SType 'Zero tx -> Expr 'Zero '[] ('IOType tx) -> m r)
    -- ^ BindIO :@ _ :$ _
    -> (t :~: ('PointerType 'ReadPointer ('TypeVar 'Zero)
        :-> 'IOType ('TypeVar 'Zero)) -> m r)
    -- ^ LoadPointer
    -> (t :~: ('PointerType 'WritePointer ('TypeVar 'Zero) :-> 'TypeVar 'Zero
        :-> 'IOType UnitType) -> m r)
    -- ^ StorePointer
    -> (t :~: ('TypeVar 'Zero :-> 'TypeVar 'Zero :-> 'TypeVar 'Zero)
        -> LLVM.Operand -> m r)
    -- ^ TestBit
    -> (LLVM.Operand -> Expr 'Zero '[] ('Forall t)
        -> Expr 'Zero '[] ('Forall t) -> m r)
    -- ^ TestBit _ :@ Forall t :$ _ :$ _
    -> Expr 'Zero '[] ('Forall t) -> m r
foldForall typeLam pureIO bindIO bindIO2 loadPointer storePointer testBit
    testBit3 = \case
    Var vidx -> absurd $ zeroNoLT vidx Refl
    App ef ex -> foldArrow
        (\tx -> foldForall typeLam pureIO bindIO bindIO2 loadPointer
                storePointer testBit testBit3
            . substituteExpr SZero (tx :^ SNil) SZero ex)
        (\case {})
        (\Refl Refl tx -> bindIO2 Refl tx ex)
        (\_ -> \case {})
        (\_ -> \case {})
        (\_ -> \case {})
        (\case {})
        (\Refl Refl _ _ (_ :^ ltargs) _ -> case ltargs of {})
        (\_ -> \case {})
        (\_ -> \case {})
        (\_ -> \case {})
        (\case {})
        (\Refl op ey -> testBit3 op ey ex)
        (\op ey ez -> testBit3 op (ey :$ ex) (ez :$ ex))
        ef
    TypeApp ef tx -> foldForall
        (foldForall typeLam pureIO bindIO bindIO2 loadPointer storePointer
            testBit testBit3 . substituteExprType SZero SNil SZero tx)
        (\case {})
        (\case {})
        (\case {})
        (\case {})
        (\case {})
        (\case {})
        (\op ey ez -> testBit3 op (ey :@ tx) (ez :@ tx))
        ef
    TypeLam ex -> typeLam ex
    PureIO -> pureIO Refl
    BindIO -> bindIO Refl
    LoadPointer -> loadPointer Refl
    StorePointer -> storePointer Refl
    Call _ _ ltargs _ -> case ltargs of {}
    TestBit ex -> emitExprOp ex >>= testBit Refl

-- | Folds an @IO@ value using the given continuations for each possible value.
foldIO
    :: forall t sig r m. Has IRBuilder sig m
    => (forall lt. t :~: 'LlvmType lt -> SLlvmType lt -> m r)
    -- ^ LLVM-typed expressions
    -> (Expr 'Zero '[] t -> m r)
    -- ^ Pure expressions
    -> (LLVM.Operand -> Expr 'Zero '[] ('IOType t)
        -> Expr 'Zero '[] ('IOType t) -> m r)
    -- ^ TestBit _ :@ IOType t :$ _ :$ _
    -> Expr 'Zero '[] ('IOType t) -> m r
foldIO llvmIO pureIO testBit3 = \case
    Var vidx -> absurd $ zeroNoLT vidx Refl
    App ef ex -> foldArrow
        (\tx -> foldIO llvmIO pureIO testBit3
            . substituteExpr SZero (tx :^ SNil) SZero ex)
        (\Refl -> pureIO ex)
        (\_ -> \case {})
        (\Refl Refl tx ey ty -> foldIO
            (\Refl lt -> do
                op <- emitExprIO ey
                foldIO llvmIO pureIO testBit3 $ ex :$ LlvmOperand lt op)
            (\ez -> foldIO llvmIO pureIO testBit3 $ ex :$ ez)
            (\op et ef' -> foldIO llvmIO pureIO testBit3
                $ withProof (subIncElim SZero SZero ty tx Refl)
                $ TestBit (LlvmOperand (SLlvmInt $ SSucc SZero) op)
                :@ SIOType ty
                :$ (BindIO :@ tx :$ et :@ ty :$ ex)
                :$ (BindIO :@ tx :$ ef' :@ ty :$ ex))
            ey)
        (\Refl Refl _ -> foldPointer
            (\tx pop -> do
                let lt = sMarshall tx
                op <- emitInstr (llvmType lt) $ LLVM.Load True pop Nothing 1 []
                pureIO $ marshallIn SZero SNil tx $ LlvmOperand lt op)
            ex)
        (\_ -> \case {})
        (\Refl ey -> foldPointer
            (\tx pop -> do
                op <- emitExprOp $ marshallOut SZero SNil tx ex
                emitInstrVoid $ LLVM.Store True pop op Nothing 1 []
                pureIO $ TypeLam $ STypeVar SZero :\ Var SZero)
            ey)
        (\Refl Refl _ _ (_ :^ SNil) ltret -> llvmIO Refl ltret)
        (\_ -> \case {})
        (\_ -> \case {})
        (\_ -> \case {})
        (\case {})
        (\Refl op ey -> testBit3 op ey ex)
        (\op ey ez -> testBit3 op (ey :$ ex) (ez :$ ex))
        ef
    TypeApp ef tx -> foldForall
        (foldIO llvmIO pureIO testBit3 . substituteExprType SZero SNil SZero tx)
        (\case {})
        (\case {})
        (\case {})
        (\case {})
        (\case {})
        (\case {})
        (\op ey ez -> testBit3 op (ey :@ tx) (ez :@ tx))
        ef
    LlvmIO lt _ -> llvmIO Refl lt
    Call _ _ SNil ltret -> llvmIO Refl ltret

-- | Folds a pointer using the given continuations for each possible value.
foldPointer
    :: forall pk tx sig r m. Has IRBuilder sig m
    => ((MarshallableType tx, IsOpType (Marshall tx) ~ 'True)
        => SType 'Zero tx -> LLVM.Operand -> m r)
    -> Expr 'Zero '[] ('PointerType pk tx) -> m r
foldPointer addr = \case
    Var vidx -> absurd $ zeroNoLT vidx Refl
    App ef ex -> foldArrow
        (\tx -> foldPointer addr . substituteExpr SZero (tx :^ SNil) SZero ex)
        (\case {})
        (\_ -> \case {})
        (\_ -> \case {})
        (\_ -> \case {})
        (\_ -> \case {})
        (\case {})
        (\Refl Refl _ _ (_ :^ ltargs) _ -> case ltargs of {})
        (\_ -> \case {})
        (\_ -> \case {})
        (\_ -> \case {})
        (\case {})
        (\Refl op ey -> emitSelect' op ey ex)
        (\op ey ez -> emitSelect' op (ey :$ ex) (ez :$ ex))
        ef
    TypeApp ef tx -> foldForall
        (foldPointer addr . substituteExprType SZero SNil SZero tx)
        (\case {})
        (\case {})
        (\case {})
        (\case {})
        (\case {})
        (\case {})
        (\op ey ez -> emitSelect' op (ey :@ tx) (ez :@ tx))
        ef
    Addr addr' _ tx -> addr tx $ llvmAddress (sMarshall tx) addr'
    Call _ _ ltargs _ -> case ltargs of {}
  where
    emitSelect'
        :: LLVM.Operand
        -> Expr 'Zero '[] ('PointerType pk tx)
        -> Expr 'Zero '[] ('PointerType pk tx)
        -> m r
    emitSelect' opc et ef = foldPointer (\_ opt -> foldPointer (\_ opf -> do
        opr <- emitInstr (llvmType $ sMarshall tx)
            $ LLVM.Select opc opt opf []
        addr tx opr
        ) ef) et
      where
        SPointerType _ tx = exprType SZero SNil et

-- | Converts an 'LlvmType' into an LLVM type in the LLVM AST.
llvmType :: SLlvmType t -> LLVM.Type
llvmType = \case
    SLlvmInt size -> case sCmpNat size SZero of
        SLT -> absurd $ zeroNoLT size Refl
        SEQ -> LLVM.VoidType
        SGT -> LLVM.IntegerType $ fromIntegral $ toNatural size

-- | Converts a foreign name to a name in the LLVM AST.
llvmForeignName :: ForeignName -> LLVM.Name
llvmForeignName (ForeignName t) = LLVM.Name $ toShortByteString t

{-|
    Converts an address to an LLVM operand representing that address. The given
    'LlvmType' is used to determine the type of the operand.
-}
llvmAddress :: SLlvmType lt -> Address -> LLVM.Operand
llvmAddress lt (Address addr) = LLVM.ConstantOperand
    $ LLVM.Constant.IntToPtr (LLVM.Constant.Int (log2 addr) $ fromIntegral addr)
    $ LLVM.Type.ptr $ llvmType lt
  where
    log2 0 = error "Address is 0"
    log2 n = fromIntegral $ naturalLog2 n + 1

-- GHC gives a nonexhaustive pattern warning if this is inlined. :/
-- | Calls a continuation with a proof relating 'AllIsOpType' and 'IsOpType'.
withAllIsOpTypeProof
    :: AllIsOpType (lt ': lts) ~ 'True => SLlvmType lt -> proxy lts
    -> ((IsOpType lt ~ 'True, AllIsOpType lts ~ 'True) => r')
    -> r'
withAllIsOpTypeProof lt _ x = case sIsOpType lt of
    STrue -> x
