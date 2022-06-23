{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Functions to convert an Elemental program into an interaction net.
module Language.Elemental.Emit
    ( emitProgram
    , emitDeclScope
    , emitDecl
    , emitExpr
    , backendType
    , backendForeignName
    ) where

import Control.Algebra ((:+:))
import Control.Carrier.State.Church (State, get, put, runState)
import Control.Monad.Trans.Class (lift)
import Data.DList (DList, toList)
import Data.Functor.Const (Const(..), getConst)
import Data.Text.Short (toShortByteString)

import Control.Carrier.ModuleBuilder
import Language.Elemental.AST.Decl
import Language.Elemental.AST.Expr
import Language.Elemental.AST.Program
import Language.Elemental.AST.Type
import Language.Elemental.Backend qualified as Backend
import Language.Elemental.InteractionNet
import Language.Elemental.Primitive
import Language.Elemental.Singleton

-- | Emits a program as an interaction net.
emitProgram
    :: HasRewriter sig m
    => Program -> m [Backend.ForeignNamed Backend.External]
emitProgram (Program decls) = toList <$> emitDeclScope SNil SNil decls

-- | Emits a list of declarations.
emitDeclScope
    :: (HasRewriter sig m)
    => SList (SType 'Zero) scope -> SList (Const (Ref -> m ())) scope
    -> DeclScope scope rest
    -> m (DList (Backend.ForeignNamed Backend.External))
emitDeclScope scopeTypes scope = \case
    DeclNil -> pure mempty
    DeclCons decl decls -> case declType scopeTypes decl of
        SNothing -> do
            exts <- emitDecl scopeTypes scope () decl
            (exts <>) <$> emitDeclScope scopeTypes scope decls
        SJust t -> do
            rn0 <- newNode $ AppNode () () ()
            rn1 <- newNode $ LamNode () () ()
            linkNodes (Ref rn0 0) (Ref rn1 0)
            propagate2 (Ref rn0 2) (Ref rn1 2) DeadNode
            exts <- emitDecl scopeTypes scope (Const $ Ref rn0 1) decl
            Ref rn1 1 >=^ scope
                $ \scope' -> (exts <>)
                    <$> emitDeclScope (t :^ scopeTypes) scope' decls

{-|
    Emits a declaration, returning the expression to add to the scope if the
    declaration adds anything to the scope.
-}
emitDecl
    :: (HasRewriter sig m)
    => SList (SType 'Zero) scope -> SList (Const (Ref -> m ())) scope
    -> FoldMaybe () (Const Ref) mt -> Decl scope mt
    -> m (DList (Backend.ForeignNamed Backend.External))
emitDecl scopeTypes scope rr = \case
    Binding expr -> mempty <$ emitExpr scope (getConst rr) expr
    ForeignImport fname t -> do
        let ltargs = sForeignArgs t
            ltret = sForeignRet t
            name = backendForeignName fname
            ext = Backend.External (backendArgs ltargs) (backendType ltret)
        emitExpr scope (getConst rr)
            $ wrapImport SZero scopeTypes t $ Call name ltargs ltret
        pure $ pure $ name Backend.:= ext
    ForeignExport fname expr -> do
        let t = exprType SZero scopeTypes expr
            ltargs = sForeignArgs t
            ltret = sForeignRet t
        names <- traverseSList newName ltargs
        let ops = zipWith Backend.Reference (backendArgs ltargs) names
            bargs = zipWith (Backend.:=) names (backendArgs ltargs)
            bret = backendType ltret
            bname = backendForeignName fname
        rn1 <- newNode $ ExternalRootNode bname bargs bret ()
        rn2 <- newNode $ AccumIONode mempty () ()
        rn3 <- newNode $ Bind0FNode () ()
        rn4 <- newNode $ AppNode () () ()
        linkNodes (Ref rn1 0) (Ref rn2 1)
        linkNodes (Ref rn2 0) (Ref rn4 2)
        linkNodes (Ref rn3 1) (Ref rn4 0)
        mkLambda (Ref rn4 1) ReturnNode
        emitExpr scope (Ref rn3 0) $ applyArgs ltret ltargs ops
            $ wrapExport SZero scopeTypes t expr
        pure mempty
    ForeignPrimitive pfin
        -> (mempty <$) $ emitExpr scope (getConst rr) $ primitiveExprs !!^ pfin
    ForeignAddress addr pk (t :: SType 'Zero t) -> let
        lt :: SType 'Zero ('BackendType (Marshall t))
        lt = SBackendType $ sMarshall t
        baddr = Backend.Address (backendType $ sMarshall t) $ getAddress addr
        li0 = SBackendInt SZero
        i0 = SBackendType li0
        in case pk of
            SReadPointer -> (mempty <$) $ emitExpr scope (getConst rr) $ BindIO
                :@ lt :$ BackendIO (sMarshall t) (Backend.Load baddr)
                :@ t :$ (lt
                    :\ marshallIn SZero (lt :^ scopeTypes) t (Var SZero))
            SWritePointer -> (mempty <$) $ emitExpr scope (getConst rr)
                $ Lam t $ BindIO
                :@ i0 :$ (BackendPIO (sMarshall t) li0 (Backend.Store baddr)
                    :$ marshallOut SZero (t :^ scopeTypes) t (Var SZero))
                :@ SUnitType :$ (i0 :\ marshallIn SZero (i0 :^ t :^ scopeTypes)
                    SUnitType (Var SZero))
  where
    traverseSList :: Applicative f => f a -> SList sing as -> f [a]
    traverseSList _ SNil = pure []
    traverseSList f (_ :^ xs) = (:) <$> f <*> traverseSList f xs

    backendArgs :: SList SBackendType lts -> [Backend.Type]
    backendArgs SNil = []
    backendArgs (lt :^ lts) = backendType lt : backendArgs lts
    
    applyArgs
        :: AllIsOpType ltargs ~ 'True
        => proxy ltret -> SList SBackendType ltargs -> [Backend.Operand]
        -> Expr 'Zero scope (BuildForeignType ltargs ltret)
        -> Expr 'Zero scope ('IOType ('BackendType ltret))
    applyArgs _ SNil [] expr = expr
    applyArgs t (ltarg :^ ltargs) (op : ops) expr
        = withAllIsOpTypeProof ltarg ltargs
        $ applyArgs t ltargs ops
        $ expr :$ BackendOperand ltarg op
    applyArgs _ SNil ops _ = error $ "emitDecl: "
        <> show (length ops) <> " excess operands"
    applyArgs _ ltargs [] _ = error $ "emitDecl: "
        <> show (toNatural $ sLength ltargs) <> " missing operands"

emitExpr
    :: forall tscope scope tx sig m. HasRewriter sig m
    => SList (Const (Ref -> m ())) scope -> Ref -> Expr tscope scope tx -> m ()
emitExpr scope rr = \case
    Var vidx -> mkBox vidx >>= getConst (scope !!^ vidx)
    App ef ex -> do
        rn1 <- newNode $ AppNode () () ()
        emitExpr scope (Ref rn1 0) ef
        emitExpr scope (Ref rn1 1) ex
        linkNodes rr $ Ref rn1 2
    TypeApp ef _ -> emitExpr scope rr ef
    Lam _ ey -> do
        rn1 <- newNode $ LamNode () () ()
        Ref rn1 1 >=^ scope $ \scope' -> emitExpr scope' (Ref rn1 2) ey
        linkNodes rr $ Ref rn1 0
    TypeLam ex -> emitExpr (coerceScope scope) rr ex
    Addr addr _ tx -> do
        rn1 <- newNode $ OperandNode (Backend.Address
            (backendType $ sMarshall tx) $ getAddress addr) ()
        linkNodes rr $ Ref rn1 0
    BackendOperand _ op -> do
        rn1 <- newNode $ OperandNode op ()
        linkNodes rr $ Ref rn1 0
    BackendIO _ instr -> propagate1 rr $ IOContNode instr
    BackendPIO _ _ pio
        -> mkLambda rr $ IOPNode (Backend.Partial (SSucc SZero) pio)
    PureIO -> do
        rn1 <- newNode $ LamNode () () ()
        rn2 <- newNode $ IOPureNode () ()
        rn3 <- newNode $ LamNode () () ()
        rn4 <- newNode $ AppNode () () ()
        rn5 <- newNode $ BoxNode 0 () ()
        linkNodes (Ref rn1 1) (Ref rn5 0)
        linkNodes (Ref rn1 2) (Ref rn2 0)
        linkNodes (Ref rn2 1) (Ref rn3 0)
        linkNodes (Ref rn3 1) (Ref rn4 0)
        linkNodes (Ref rn3 2) (Ref rn4 2)
        linkNodes (Ref rn4 1) (Ref rn5 1)
        linkNodes rr $ Ref rn1 0
    BindIO -> mkLambda rr Bind0BNode
    LoadPointer -> do
        rn1 <- newNode $ LamNode () () ()
        linkNodes (Ref rn1 1) (Ref rn1 2)
        linkNodes rr $ Ref rn1 0
    StorePointer -> do
        rn1 <- newNode $ LamNode () () ()
        linkNodes (Ref rn1 1) (Ref rn1 2)
        linkNodes rr $ Ref rn1 0
    Call fname SNil tret -> propagate1 rr $ IOContNode
        $ Backend.Call (backendType tret) (Backend.ExternalName fname) []
    Call fname ltargs@(_ :^ _) tret -> do
        let callp = Backend.Partial len $ withVarargs len
                $ Backend.Call (backendType tret) (Backend.ExternalName fname)
            len = sLength ltargs
        mkLambda rr $ IOPNode callp
    IsolateBit bidx ssize -> do
        let opp = Backend.Partial (SSucc SZero) $ mkIsolateBit size bidx'
            size = fromIntegral $ toNatural ssize
            bidx' = fromIntegral $ toNatural bidx
        mkLambda rr $ OperandPNode opp
    InsertBit ssize -> do
        let opp = Backend.Partial (SSucc $ SSucc SZero) $ Backend.InsertBit size
            size = fromIntegral $ toNatural ssize
        mkLambda rr $ OperandPNode opp
    TestBit -> mkLambda rr Branch0Node
  where
    coerceScope :: SList (Const a) as -> SList (Const a) (IncrementAll 'Zero as)
    coerceScope SNil = SNil
    coerceScope (Const a :^ as) = Const a :^ coerceScope as

    withVarargs :: SNat n -> ([a] -> b) -> Backend.FoldArrow n a b
    withVarargs SZero f = f []
    withVarargs (SSucc n) f = \x -> withVarargs n $ f . (x :)

    mkBox :: SNat n -> m Ref
    mkBox SZero = pure rr
    mkBox (SSucc n) = do
        r1 <- mkBox n
        rn2 <- newNode $ BoxNode 0 () ()
        linkNodes r1 $ Ref rn2 1
        pure $ Ref rn2 0

    mkIsolateBit :: Int -> Int -> Backend.Operand -> Backend.Operand
    mkIsolateBit size bidx (Backend.InsertBit _ oph opt)
        | bidx == 0 = oph
        | otherwise = mkIsolateBit size (pred bidx) opt
    mkIsolateBit size bidx op = Backend.IsolateBit size bidx op

(>=^)
    :: HasRewriter sig m
    => Ref -> SList (Const (Ref -> m ())) scope
    -> (forall n. Algebra (State (Ref, Maybe Ref) :+: sig) n
        => SList (Const (Ref -> n ())) (tx ': scope) -> n r)
    -> m r
(r0 >=^ sc) f = runState @(Ref, Maybe Ref) finish (r0, Nothing) $ f
    $ Const dup :^ sMap (Const . (.) lift . getConst) sc
  where
    sMap :: (forall a. f a -> g a) -> SList f as -> SList g as
    sMap _ SNil = SNil
    sMap nt (a :^ as) = nt a :^ sMap nt as

    dup :: (HasRewriter sig m, Has (State (Ref, Maybe Ref)) sig m)
        => Ref -> m ()
    dup r1 = do
        (r2, mr3) <- get @(Ref, Maybe Ref)
        case mr3 of
            Nothing -> put (r2, Just r1)
            Just r3 -> do
                rn4 <- newNode $ DupNode 0 () () ()
                linkNodes r2 $ Ref rn4 0
                linkNodes r3 $ Ref rn4 1
                put (Ref rn4 2, Just r1)

    finish :: HasRewriter sig m => (Ref, Maybe Ref) -> r -> m r
    finish (r1, mr2) r = r <$ maybe (propagate1 r1 DeadNode) (linkNodes r1) mr2

-- | Converts a t'BackendType' into a backend type in the backend AST.
backendType :: SBackendType t -> Backend.Type
backendType (SBackendInt size) = Backend.IntType $ fromIntegral $ toNatural size

-- | Converts a foreign name to a name in the backend AST.
backendForeignName :: ForeignName -> Backend.ForeignName
backendForeignName (ForeignName t) = Backend.ForeignName $ toShortByteString t

-- GHC gives a nonexhaustive pattern warning if this is inlined. :/
-- | Calls a continuation with a proof relating 'AllIsOpType' and 'IsOpType'.
withAllIsOpTypeProof
    :: AllIsOpType (lt ': lts) ~ 'True => SBackendType lt -> proxy lts
    -> ((IsOpType lt ~ 'True, AllIsOpType lts ~ 'True) => r')
    -> r'
withAllIsOpTypeProof lt _ x = case sIsOpType lt of
    STrue -> x

