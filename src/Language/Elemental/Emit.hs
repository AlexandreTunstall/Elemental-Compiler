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
    :: HasRewriter sig m => Program -> m [Backend.Named Backend.External]
emitProgram (Program decls) = toList <$> emitDeclScope SNil SNil decls

-- | Emits a list of declarations.
emitDeclScope
    :: (HasRewriter sig m)
    => SList (SType 'Zero) scope -> SList (Const (Ref -> m ())) scope
    -> DeclScope scope rest -> m (DList (Backend.Named Backend.External))
emitDeclScope scopeTypes scope = \case
    DeclNil -> pure mempty
    DeclCons decl decls -> case declType scopeTypes decl of
        SNothing -> do
            exts <- emitDecl scopeTypes scope () decl
            (exts <>) <$> emitDeclScope scopeTypes scope decls
        SJust t -> do
            rn0 <- newNode $ const $ AppNode () () ()
            rn1 <- newNode $ const $ LamNode () () ()
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
    -> m (DList (Backend.Named Backend.External))
emitDecl scopeTypes scope rr = \case
    Binding expr -> mempty <$ emitExpr scope (getConst rr) expr
    ForeignImport fname t -> do
        let ltargs = sForeignArgs t
            ltret = sForeignRet t
            name = backendForeignName fname
        emitExpr scope (getConst rr)
            $ wrapImport SZero scopeTypes t $ Call name ltargs ltret
        pure $ pure $ name
            Backend.:= Backend.External (backendArgs ltargs) (backendType ltret)
    ForeignExport fname expr -> do
        let t = exprType SZero scopeTypes expr
            ltargs = sForeignArgs t
            ltret = sForeignRet t
        freshNums <- traverseSList newNodeIndex ltargs
        let names = Backend.Name . fromIntegral <$> freshNums
            ops = zipWith Backend.Reference (backendArgs ltargs) names
            args = zipWith (Backend.:=) names (backendArgs ltargs)
        rn1 <- newNode $ const $ RootNode (backendForeignName fname) args ()
        emitExpr scope (Ref rn1 0) $ applyArgs ltret ltargs ops
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
                :@ lt :$ BackendIO (sMarshall t)
                    (Backend.Body mempty $ Backend.Load baddr)
                :@ t :$ Lam lt (PureIO :@ t
                    :$ marshallIn SZero (lt :^ scopeTypes) t (Var SZero))
            SWritePointer -> (mempty <$) $ emitExpr scope (getConst rr)
                $ Lam t $ BindIO
                :@ i0 :$ (BackendPIO (sMarshall t) li0 (Backend.Store baddr)
                    :$ marshallOut SZero (t :^ scopeTypes) t (Var SZero))
                :@ SUnitType :$ Lam i0 (PureIO :@ SUnitType
                    :$ marshallIn SZero (i0 :^ t :^ scopeTypes) SUnitType
                        (Var SZero))
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
    Var vidx -> getConst (scope !!^ vidx) rr
    App ef ex -> do
        rn1 <- newNode $ const $ AppNode () () ()
        emitExpr scope (Ref rn1 0) ef
        emitExpr scope (Ref rn1 1) ex
        linkNodes rr $ Ref rn1 2
    TypeApp ef _ -> emitExpr scope rr ef
    Lam _ ey -> do
        rn1 <- newNode $ const $ LamNode () () ()
        Ref rn1 1 >=^ scope $ \scope' -> emitExpr scope' (Ref rn1 2) ey
        linkNodes rr $ Ref rn1 0
    TypeLam ex -> emitExpr (coerceScope scope) rr ex
    Addr addr _ tx -> do
        rn1 <- newNode $ const $ OperandNode (Backend.Address
            (backendType $ sMarshall tx) $ getAddress addr) ()
        linkNodes rr $ Ref rn1 0
    BackendOperand _ op -> do
        rn1 <- newNode $ const $ OperandNode op ()
        linkNodes rr $ Ref rn1 0
    BackendIO _ body -> do
        rn1 <- newNode $ const $ IONode body ()
        linkNodes rr $ Ref rn1 0
    BackendPIO _ _ pio
        -> mkLambda rr $ IOPNode (Backend.Partial (SSucc SZero) pio)
    PureIO -> mkLambda rr Pure0Node
    BindIO -> mkLambda rr Bind0Node
    LoadPointer -> do
        rn1 <- newNode $ const $ LamNode () () ()
        linkNodes (Ref rn1 1) (Ref rn1 2)
        linkNodes rr $ Ref rn1 0
    StorePointer -> do
        rn1 <- newNode $ const $ LamNode () () ()
        linkNodes (Ref rn1 1) (Ref rn1 2)
        linkNodes rr $ Ref rn1 0
    Call name SNil tret -> do
        let body = Backend.Body
                { Backend.bodyInstrs = mempty
                , Backend.bodyTerm = Backend.Call (backendType tret) name []
                }
        propagate1 rr $ IONode body
    Call name ltargs@(_ :^ _) tret -> do
        let callp = Backend.Partial len $ withVarargs len
                $ Backend.Call (backendType tret) name
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
    TestBit -> mkLambda rr $ Branch0Node $ Level []
  where
    coerceScope :: SList (Const a) as -> SList (Const a) (IncrementAll 'Zero as)
    coerceScope SNil = SNil
    coerceScope (Const a :^ as) = Const a :^ coerceScope as

    withVarargs :: SNat n -> ([a] -> b) -> Backend.FoldArrow n a b
    withVarargs SZero f = f []
    withVarargs (SSucc n) f = \x -> withVarargs n $ f . (x :)

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
                rn4 <- newNode $ \rn4
                    -> DupNode Fanout (Level [rn4]) mempty () () ()
                linkNodes r2 $ Ref rn4 0
                linkNodes r3 $ Ref rn4 1
                put (Ref rn4 2, Just r1)

    finish :: HasRewriter sig m => (Ref, Maybe Ref) -> r -> m r
    finish (r1, mr2) r = r <$ maybe (propagate1 r1 DeadNode) (linkNodes r1) mr2

-- | Converts a t'BackendType' into a backend type in the backend AST.
backendType :: SBackendType t -> Backend.Type
backendType (SBackendInt size) = Backend.IntType $ fromIntegral $ toNatural size

-- | Converts a foreign name to a name in the backend AST.
backendForeignName :: ForeignName -> Backend.Name
backendForeignName (ForeignName t) = Backend.ExternalName $ toShortByteString t

-- GHC gives a nonexhaustive pattern warning if this is inlined. :/
-- | Calls a continuation with a proof relating 'AllIsOpType' and 'IsOpType'.
withAllIsOpTypeProof
    :: AllIsOpType (lt ': lts) ~ 'True => SBackendType lt -> proxy lts
    -> ((IsOpType lt ~ 'True, AllIsOpType lts ~ 'True) => r')
    -> r'
withAllIsOpTypeProof lt _ x = case sIsOpType lt of
    STrue -> x

