{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-|
    Interaction net based evaluator for compiling Elemental.

    This evaluator makes the following assumptions about the input:
     - The program is total.
     - There are no edges between ports whose types don't unify.
     - IO is never duplicated, eliminated, or branched without a continuation.

    The easiest way of using the evaluator is to use 'compileINet'.

    Should you choose to manually generate an interaction net instead of using
    existing functions, be careful not to violate these assumptions, else your
    net may fail to evaluate.

    For better performance, the evaluator tracks redundant data about the net:
    interacting nodes are stored in 'INetPairs'. The evaluator assumes that the
    data stored there is correct and complete, and it will neither verify that a
    pair is interacting nor look elsewhere for missing interacting pairs. Hence,
    it is highly recommended to only use the exported helper functions, never
    manually change 'INet' or 'INetPairs', and always keep an 'INet' and its
    'INetPairs' together.
-}
module Language.Elemental.InteractionNet
    ( INet(..)
    , _INet
    , INetPairs(..)
    , _INetPairs
    , INetSize(..)
    , _INetSize
    , INetF(..)
    , Ref(..)
    , Level(..)
    , Preaction(..)
    , Renames
    , HasRewriter
    , compileINet
    , reduce
    , propagate1
    , propagate2
    , mkLambda
    , newNode
    , newNodeIndex
    , linkNodes
    -- * Debugging
    , TraceRewrite(..)
    , traceRewrite
    ) where

import Control.Algebra (Has, send)
import Control.Carrier.Writer.Church (Writer, execWriter, tell)
import Control.Effect.State (State, get, gets, modify)
import Control.Lens.At (At(at), Index, Ixed(ix), IxValue)
import Control.Lens.Cons (_head)
import Control.Lens.Fold (IndexedFold, filtered, folded, imapMOf_, (^..), (^?))
import Control.Lens.Getter (to)
import Control.Lens.Indexed (Indexed(Indexed), indexing)
import Control.Lens.Iso (Iso', iso)
import Control.Lens.Setter ((.~), (%~), (?~))
import Control.Lens.Traversal (traversed)
import Control.Lens.Wrapped (_Wrapped)
import Data.Bifunctor (second)
import Data.DList (DList, toList)
import Data.Foldable (find)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (nub)
import Data.Map.Lazy qualified as M
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Prettyprinter

import Language.Elemental.Backend qualified as B
import Language.Elemental.Singleton

newtype INet = INet { unINet :: IM.IntMap (INetF Ref) }
    deriving newtype (Semigroup, Monoid)

instance Pretty INet where
    pretty = concatWith mkLine . fmap go . IM.assocs . unINet
      where
        go (idx, v) = pretty idx <+> "=" <+> pretty v
        mkLine a b = a <> line <> b

instance Ixed INet where
    ix idx = _INet . ix idx
    {-# INLINE ix #-}

instance At INet where
    at idx = _INet . at idx
    {-# INLINE at #-}

type instance Index INet = Int
type instance IxValue INet = INetF Ref

_INet :: Iso' INet (IM.IntMap (INetF Ref))
_INet = iso unINet INet
{-# INLINE _INet #-}

newtype INetPairs = INetPairs { unINetPairs :: IS.IntSet }
    deriving newtype (Semigroup, Monoid)

_INetPairs :: Iso' INetPairs IS.IntSet
_INetPairs = iso unINetPairs INetPairs
{-# INLINE _INetPairs #-}

data Ref = Ref
    { refNode :: Int
    , refPort :: Int
    } deriving stock (Eq, Ord, Show)

instance Pretty Ref where
    pretty r0 = pretty (refNode r0) <> ":" <> pretty (refPort r0)

data INetF a
    -- | (a -> b, a, b)
    = AppNode a a a
    -- | (a -> b, a, b)
    | LamNode a a a
    -- | (a, a, a)
    | DupNode Level Renames a a a
    -- | Void (i.e. any type)
    | DeadNode a
    -- | (a, a) and the non-principal node is in a new box.
    | BoxNode Level a a
    -- FFI
    -- | IO i{n}
    | RootNode B.Name [B.Named B.Type] a
    -- | i{n}
    | OperandNode B.Operand a
    -- | (i{n}, {... ->} i{n})
    | OperandPNode (B.Partial B.Operand) a a
    -- | IO i{n}
    | IONode B.Body a
    -- | (i{n}, {... ->} i{n})
    | IOPNode (B.Partial B.Instruction) a a
    -- | (IO a, IO a)
    | IOContNode Preaction a a
    -- | (a, IO a)
    | Pure0Node a a
    -- | (IO a, (a -> IO b) -> IO b)
    | Bind0Node a a
    -- | (IO b, IO b)
    | Bind1Node Preaction a a
    -- | (i1, a -> a -> a)
    | Branch0Node Level a a
    -- | (a, a -> a)
    | Branch1Node Level B.Operand a a
    -- | (IO a, a, IO a)
    | Branch2BNode Level B.Operand Preaction a a a
    -- | (IO (a -> b), a -> b, IO (a -> b))
    | Branch2PNode Level B.Operand a a a
    -- | (IO (a -> b), a -> b, a -> b)
    | Branch3PNode Level B.Operand a a a
    deriving stock (Foldable, Functor, Traversable)

instance Pretty a => Pretty (INetF a) where
    pretty (AppNode r0 r1 r2) = "App" <+> pretty r0 <+> pretty r1 <+> pretty r2
    pretty (LamNode r0 r1 r2) = "Lam" <+> pretty r0 <+> pretty r1 <+> pretty r2
    pretty (DupNode lvl _ r0 r1 r2)
        = "Dup" <+> pretty lvl <+> pretty r0 <+> pretty r1 <+> pretty r2
    pretty (DeadNode r0) = "Dead" <+> pretty r0
    pretty (BoxNode lvl r0 r1)
        = "Box" <+> pretty lvl <+> pretty r0 <+> pretty r1
    pretty (RootNode name args r0)
        = "Root" <+> pretty r0 <+> pretty name <+> tupled (pretty <$> args)
    pretty (OperandNode op r0) = "Operand" <+> pretty r0 <+> pretty op
    pretty (OperandPNode opp r0 r1)
        = "OperandP" <+> pretty r0 <+> pretty r1 <+> pretty opp
    pretty (IONode b r0) = "IO" <+> pretty r0 <> nest 4 (line <> pretty b)
    pretty (IOPNode iop r0 r1)
        = "IOP" <+> pretty r0 <+> pretty r1 <+> pretty iop
    pretty (IOContNode nbs r0 r1)
        = "IOCont" <+> pretty r0 <+> pretty r1 <> nest 4 (line <> pretty nbs)
    pretty (Pure0Node r0 r1) = "Pure0" <+> pretty r0 <+> pretty r1
    pretty (Bind0Node r0 r1) = "Bind0" <+> pretty r0 <+> pretty r1
    pretty (Bind1Node nbs r0 r1)
        = "Bind1" <+> pretty r0 <+> pretty r1 <> nest 4 (line <> pretty nbs)
    pretty (Branch0Node lvl r0 r1)
        = "Branch0" <+> pretty lvl <+> pretty r0 <+> pretty r1
    pretty (Branch1Node lvl op r0 r1)
        = "Branch1" <+> pretty lvl <+> pretty r0 <+> pretty r1 <+> pretty op
    pretty (Branch2BNode lvl opc nbt r0 r1 r2)
        = "Branch2B" <+> pretty lvl <+> pretty r0 <+> pretty r1 <+> pretty r2
        <+> pretty opc <> nest 4 (line <> pretty nbt)
    pretty (Branch2PNode lvl opc r0 r1 r2)
        = "Branch2P" <+> pretty lvl <+> pretty r0 <+> pretty r1 <+> pretty r2
        <+> pretty opc
    pretty (Branch3PNode lvl opc r0 r1 r2)
        = "Branch3P" <+> pretty lvl <+> pretty r0 <+> pretty r1 <+> pretty r2
        <+> pretty opc

instance Ixed (INetF a) where
    ix idx f = indexing traverse $ Indexed go
      where
        go idx' x
          | idx == idx' = f x
          | otherwise = pure x
    {-# INLINE ix #-}

type instance Index (INetF a) = Int
type instance IxValue (INetF a) = a

newtype Level = Level { unLevel :: Int }
    deriving newtype (Enum, Eq, Ord, Num, Pretty)

-- | Instructions that should run before a continuation.
data Preaction
    = SimplePreaction B.Name B.Body
    | BranchPreaction B.Operand Preaction Preaction
    | ConcatPreaction Preaction Preaction
    | EmptyPreaction

instance Semigroup Preaction where
    (<>) = ConcatPreaction

instance Monoid Preaction where
    mempty = EmptyPreaction

instance Pretty Preaction where
    pretty (SimplePreaction name b) = pretty name <+> "=" <+> pretty b
    pretty (BranchPreaction op nb1 nb2) = "If" <+> pretty op <+> "Then"
        <> nest 4 (line <> pretty nb1) <> line <> "Else"
        <> nest 4 (line <> pretty nb2) <> line <> "End"
    pretty (ConcatPreaction nb1 nb2) = pretty nb1 <> line <> pretty nb2
    pretty EmptyPreaction = "_ = []"

type Renames = M.Map B.Name (B.Operand, B.Operand)

compileINet
    :: (HasRewriter sig m, Has TraceRewrite sig m)
    => [B.Named B.External] -> m B.Program
compileINet exts = B.Program exts . toList <$> execWriter reduce
{-# INLINABLE compileINet #-}

reduce
    :: (HasRewriter sig m, Has TraceRewrite sig m
    , Has (Writer (DList (B.Named B.Function))) sig m)
    => m ()
reduce = try *> lintFinal
  where
    go r0 = do
        net <- get
        let r1 = derefPort n0 0
            n1 = derefNode net $ refNode r1
            n0 = derefNode net $ refNode r0
        traceRewrite r0 r1 n0 n1 $ do
            reduceNode n0 n1
            safeDelete r0 n0
            safeDelete r1 n1
        try

    try :: (HasRewriter sig m, Has TraceRewrite sig m
        , Has (Writer (DList (B.Named B.Function))) sig m)
        => m ()
    try = do
        net <- get
        case net ^? _INetPairs . _Wrapped . _head of
            Nothing -> pure ()
            Just rn0 -> go $ Ref rn0 0

    isRoot :: INetF Ref -> Bool
    isRoot RootNode {} = True
    isRoot _ = False

    derefNode :: INet -> Int -> INetF Ref
    derefNode net rn0 = fromMaybe (error "reduce: missing node")
        $ net ^? ix rn0

    derefPort :: INetF Ref -> Int -> Ref
    derefPort n0 r1 = fromMaybe (error "reduce: missing port")
        $ n0 ^? ix r1

    -- Handling self-reference during reduction is far more tedious.
    safeDelete :: HasRewriter sig m => Ref -> INetF Ref -> m ()
    safeDelete (Ref rn0 _) n0 = do
        imapMOf_ targets relink n0
        modify $ _INet . at rn0 .~ Nothing
        modify $ _INetPairs %~ IS.delete rn0
      where
        targets :: IndexedFold Int (INetF Ref) Int
        targets = traversed . filtered ((== rn0) . refNode) . to refPort

        relink :: HasRewriter sig m => Int -> Int -> m ()
        relink rp1 rp2 = do
            net <- get
            let n3 = derefNode net rn0
            linkNodes (derefPort n3 rp1) (derefPort n3 rp2)

    lintFinal :: HasRewriter sig m => m ()
    lintFinal = do
        net <- get
        case find isRoot $ unINet net of
            Nothing -> pure ()
            Just _ -> error . show $ "lint: failed to reduce root"
                <> line <> pretty net
{-# INLINABLE reduce #-}

reduceNode
    :: (HasRewriter sig m, Has (Writer (DList (B.Named B.Function))) sig m)
    => INetF Ref -> INetF Ref -> m ()
-- reduceNode (AppNode _ r0 r1) (AppNode _ r2 r3)
--     = linkNodes r0 r2 *> linkNodes r1 r3
reduceNode (AppNode _ r0 r1) (LamNode _ r2 r3) = do
    rn4 <- newNode $ const $ BoxNode 0 () ()
    rn5 <- newNode $ const $ BoxNode 0 () ()
    linkNodes r0 $ Ref rn4 0
    linkNodes r1 $ Ref rn5 0
    linkNodes r2 $ Ref rn4 1
    linkNodes r3 $ Ref rn5 1
reduceNode n0@LamNode {} n1@AppNode {} = reduceNode n1 n0
reduceNode (AppNode _ r0 r1) (DupNode lvl re _ r2 r3)
    = commute2 AppNode (DupNode lvl re) r0 r1 r2 r3
reduceNode n0@DupNode {} n1@AppNode {} = reduceNode n1 n0
reduceNode (LamNode _ r0 r1) (DupNode lvl re _ r2 r3)
    = commute2 LamNode (DupNode (succ lvl) re) r0 r1 r2 r3
reduceNode n0@DupNode {} n1@LamNode {} = reduceNode n1 n0
reduceNode (DupNode lvl1 re1 _ r0 r1) (DupNode lvl2 re2 _ r2 r3)
    | lvl1 == lvl2 = linkNodes r0 r2 *> linkNodes r1 r3
    | lvl1 == 10 && lvl2 == 11 || lvl1 == 11 && lvl2 == 10 = linkNodes r0 r2 *> linkNodes r1 r3
    | otherwise = commute2 (DupNode lvl1 re1) (DupNode lvl2 re2) r0 r1 r2 r3
reduceNode (AppNode _ r0 r1) (DeadNode _) = propagate2 r0 r1 DeadNode
reduceNode n0@DeadNode {} n1@AppNode {} = reduceNode n1 n0
reduceNode (LamNode _ r0 r1) (DeadNode _) = propagate2 r0 r1 DeadNode
reduceNode n0@DeadNode {} n1@LamNode {} = reduceNode n1 n0
reduceNode (DupNode _ _ _ r0 r1) (DeadNode _) = propagate2 r0 r1 DeadNode
reduceNode n0@DeadNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DeadNode _) (DeadNode _) = pure ()
-- Book-keeping
reduceNode (AppNode _ r0 r1) (BoxNode lvl _ r2)
    = commute1 AppNode (BoxNode lvl) r0 r1 r2
reduceNode n0@BoxNode {} n1@AppNode {} = reduceNode n1 n0
reduceNode (LamNode _ r0 r1) (BoxNode lvl _ r2)
    = commute1 LamNode (BoxNode $ succ lvl) r0 r1 r2
reduceNode n0@BoxNode {} n1@LamNode {} = reduceNode n1 n0
reduceNode (DupNode lvl0 re _ r0 r1) (BoxNode lvl1 _ r2) = commute1
    (DupNode (if lvl0 < lvl1 then lvl0 else succ lvl0) re)
    (BoxNode lvl1)
    r0 r1 r2
reduceNode n0@BoxNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (BoxNode _ _ r0) (DeadNode _) = propagate1 r0 DeadNode
reduceNode n0@DeadNode {} n1@BoxNode {} = reduceNode n1 n0
reduceNode (BoxNode lvl0 _ r0) (BoxNode lvl1 _ r1)
    | lvl0 == lvl1 = linkNodes r0 r1
    | otherwise = commute0
        (BoxNode $ if lvl0 < lvl1 then lvl0 else succ lvl0)
        (BoxNode $ if lvl1 < lvl0 then lvl1 else succ lvl1)
        r0 r1
-- FFI
reduceNode (RootNode name args _) (IONode b _)
    = tell $ pure @DList $ name B.:= B.Function args b
reduceNode n0@IONode {} n1@RootNode {} = reduceNode n1 n0
reduceNode (OperandPNode opp _ r0) (OperandNode op _)
    = case B.addOperand op opp of
        Left opp' -> mkLambda r0 $ OperandPNode opp'
        Right op' -> propagate1 r0 $ OperandNode op'
reduceNode n0@OperandNode {} n1@OperandPNode {} = reduceNode n1 n0
reduceNode (IOPNode iop _ r0) (OperandNode op _)
    = case B.addOperand op iop of
        Left iop' -> mkLambda r0 $ IOPNode iop'
        Right instr -> propagate1 r0 $ IONode $ B.Body mempty instr
reduceNode n0@OperandNode {} n1@IOPNode {} = reduceNode n1 n0
reduceNode (Pure0Node _ r0) (LamNode _ r1 r2) = do
    rn3 <- newNode $ const $ IOContNode mempty () ()
    rn4 <- newNode $ const $ LamNode () () ()
    linkNodes (Ref rn3 1) (Ref rn4 0)
    linkNodes r0 $ Ref rn3 0
    linkNodes r1 $ Ref rn4 1
    linkNodes r2 $ Ref rn4 2
reduceNode n0@LamNode {} n1@Pure0Node {} = reduceNode n1 n0
reduceNode (Pure0Node _ r0) (Branch3PNode lvl opc _ r1 r2) = do
    rn3 <- newNode $ const $ IOContNode mempty () ()
    rn4 <- newNode $ const $ Branch3PNode lvl opc () () ()
    linkNodes (Ref rn3 1) (Ref rn4 0)
    linkNodes r0 $ Ref rn3 0
    linkNodes r1 $ Ref rn4 1
    linkNodes r2 $ Ref rn4 2
reduceNode n0@Branch3PNode {} n1@Pure0Node {} = reduceNode n1 n0
reduceNode (Pure0Node _ r0) (OperandNode op _)
    = propagate1 r0 $ IONode $ B.Body mempty $ B.Pure op
reduceNode n0@OperandNode {} n1@Pure0Node {} = reduceNode n1 n0
reduceNode (Pure0Node _ r0) (IONode b _) = do
    rn3 <- newNode $ const $ IOContNode mempty () ()
    rn4 <- newNode $ const $ IONode b ()
    linkNodes (Ref rn3 1) (Ref rn4 0)
    linkNodes r0 $ Ref rn3 0
reduceNode n0@IONode {} n1@Pure0Node {} = reduceNode n1 n0
reduceNode (Pure0Node _ r0) (IOContNode nbs _ r1) = do
    rn3 <- newNode $ const $ IOContNode mempty () ()
    rn4 <- newNode $ const $ IOContNode nbs () ()
    linkNodes (Ref rn3 1) (Ref rn4 0)
    linkNodes r0 $ Ref rn3 0
    linkNodes r1 $ Ref rn4 1
reduceNode n0@IOContNode {} n1@Pure0Node {} = reduceNode n1 n0
reduceNode (Bind0Node _ r0) (IOContNode nbs _ r1) = do
    rn2 <- newNode $ const $ Bind1Node nbs () ()
    rn3 <- newNode $ const $ AppNode () () ()
    rn4 <- newNode $ const $ LamNode () () ()
    linkNodes (Ref rn2 0) (Ref rn3 2)
    linkNodes (Ref rn2 1) (Ref rn4 2)
    linkNodes (Ref rn3 0) (Ref rn4 1)
    linkNodes r0 $ Ref rn4 0
    linkNodes r1 $ Ref rn3 1
reduceNode n0@IOContNode {} n1@Bind0Node {} = reduceNode n1 n0
reduceNode (Bind0Node _ r0) (IONode b _) = do
    rn1 <- newNode $ \rn1 -> Bind1Node (SimplePreaction (mkName rn1) b) () ()
    let t = B.instrType $ B.bodyTerm b
    rn2 <- newNode $ const $ OperandNode (mkRef t rn1) ()
    rn3 <- newNode $ const $ AppNode () () ()
    rn4 <- newNode $ const $ LamNode () () ()
    linkNodes (Ref rn1 0) (Ref rn3 2)
    linkNodes (Ref rn1 1) (Ref rn4 2)
    linkNodes (Ref rn2 0) (Ref rn3 1)
    linkNodes (Ref rn3 0) (Ref rn4 1)
    linkNodes r0 $ Ref rn4 0
reduceNode n0@IONode {} n1@Bind0Node {} = reduceNode n1 n0
reduceNode (Bind1Node nbs _ r0) (IONode b2 _)
    = propagate1 r0 $ IONode $ applyCont nbs b2
reduceNode n0@IONode {} n1@Bind1Node {} = reduceNode n1 n0
reduceNode (Bind1Node nbs1 _ r0) (IOContNode nbs2 _ r1) = do
    rn2 <- newNode $ const $ IOContNode (nbs1 <> nbs2) () ()
    linkNodes r0 $ Ref rn2 0
    linkNodes r1 $ Ref rn2 1
reduceNode n0@IOContNode {} n1@Bind1Node {} = reduceNode n1 n0
reduceNode (Branch0Node lvl _ r0) (OperandNode op _)
    = mkLambda r0 $ Branch1Node lvl op
reduceNode n0@OperandNode {} n1@Branch0Node {} = reduceNode n1 n0
reduceNode (Branch1Node lvl op _ r0) (AppNode _ r1 r2) = do
    rn3 <- newNode $ const $ Branch2PNode lvl op () () ()
    rn4 <- newNode $ const $ LamNode () () ()
    rn5 <- newNode $ const $ AppNode () () ()
    linkNodes (Ref rn3 0) (Ref rn4 1)
    linkNodes (Ref rn3 1) (Ref rn5 0)
    linkNodes (Ref rn3 2) (Ref rn4 2)
    linkNodes r0 $ Ref rn4 0
    linkNodes r1 $ Ref rn5 1
    linkNodes r2 $ Ref rn5 2
reduceNode n0@AppNode {} n1@Branch1Node {} = reduceNode n1 n0
reduceNode (Branch1Node lvl op _ r0) (LamNode _ r1 r2) = do
    rn3 <- newNode $ const $ Branch2PNode lvl op () () ()
    rn4 <- newNode $ const $ LamNode () () ()
    rn5 <- newNode $ const $ LamNode () () ()
    linkNodes (Ref rn3 0) (Ref rn4 1)
    linkNodes (Ref rn3 1) (Ref rn5 0)
    linkNodes (Ref rn3 2) (Ref rn4 2)
    linkNodes r0 $ Ref rn4 0
    linkNodes r1 $ Ref rn5 1
    linkNodes r2 $ Ref rn5 2
reduceNode n0@LamNode {} n1@Branch1Node {} = reduceNode n1 n0
reduceNode (Branch1Node lvl0 opc0 _ r0) (Branch3PNode lvl1 opc1 _ r1 r2) = do
    rn3 <- newNode $ const $ Branch2PNode lvl0 opc0 () () ()
    rn4 <- newNode $ const $ LamNode () () ()
    rn5 <- newNode $ const $ Branch3PNode lvl1 opc1 () () ()
    linkNodes (Ref rn3 0) (Ref rn4 1)
    linkNodes (Ref rn3 1) (Ref rn5 0)
    linkNodes (Ref rn3 2) (Ref rn4 2)
    linkNodes r0 $ Ref rn4 0
    linkNodes r1 $ Ref rn5 1
    linkNodes r2 $ Ref rn5 2
reduceNode n0@Branch3PNode {} n1@Branch1Node {} = reduceNode n1 n0
reduceNode (Branch1Node _ opc _ r0) (OperandNode opt _) = mkLambda r0
    $ OperandPNode $ B.Partial (SSucc SZero) $ mkSelect opc opt
reduceNode n0@OperandNode {} n1@Branch1Node {} = reduceNode n1 n0
reduceNode (Branch1Node lvl op _ r0) (IOContNode nbs _ r1) = do
    rn3 <- newNode $ const $ Branch2BNode lvl op nbs () () ()
    rn4 <- newNode $ const $ LamNode () () ()
    linkNodes (Ref rn3 0) (Ref rn4 1)
    linkNodes (Ref rn3 2) (Ref rn4 2)
    linkNodes r0 $ Ref rn4 0
    linkNodes r1 $ Ref rn3 1
reduceNode n0@IOContNode {} n1@Branch1Node {} = reduceNode n1 n0
reduceNode (Branch2BNode lvl op nbs1 _ r0 r1) (IOContNode nbs2 _ r2) = do
    let nbs = BranchPreaction op nbs1 nbs2
    rn3 <- newNode $ const $ Branch1Node lvl op () ()
    rn4 <- newNode $ const $ AppNode () () ()
    rn5 <- newNode $ const $ IOContNode nbs () ()
    linkNodes (Ref rn3 1) (Ref rn4 0)
    linkNodes (Ref rn4 2) (Ref rn5 1)
    linkNodes r0 $ Ref rn3 0
    linkNodes r1 $ Ref rn5 0
    linkNodes r2 $ Ref rn4 1
reduceNode n0@IOContNode {} n1@Branch2BNode {} = reduceNode n1 n0
reduceNode (Branch2PNode lvl opc _ r0 r1) (LamNode _ r2 r3) = do
    rn4 <- newNode $ const $ Branch3PNode lvl opc () () ()
    rn5 <- newNode $ const $ LamNode () () ()
    linkNodes (Ref rn4 2) (Ref rn5 0)
    linkNodes r0 $ Ref rn4 1
    linkNodes r1 $ Ref rn4 0
    linkNodes r2 $ Ref rn5 1
    linkNodes r3 $ Ref rn5 2
reduceNode n0@LamNode {} n1@Branch2PNode {} = reduceNode n1 n0
reduceNode (Branch2PNode lvl0 opc0 _ r0 r1) (Branch3PNode lvl1 opc1 _ r2 r3) = do
    rn4 <- newNode $ const $ Branch3PNode lvl0 opc0 () () ()
    rn5 <- newNode $ const $ Branch3PNode lvl1 opc1 () () ()
    linkNodes (Ref rn4 2) (Ref rn5 0)
    linkNodes r0 $ Ref rn4 1
    linkNodes r1 $ Ref rn4 0
    linkNodes r2 $ Ref rn5 1
    linkNodes r3 $ Ref rn5 2
reduceNode n0@Branch3PNode {} n1@Branch2PNode {} = reduceNode n1 n0
reduceNode (Branch3PNode lvl opc _ r0 r1) (AppNode _ r2 r3) = do
    rn4 <- newNode $ const $ AppNode () () ()
    rn5 <- newNode $ const $ AppNode () () ()
    rn6 <- newNode $ const $ DupNode 0 mempty () () ()
    rn7 <- newNode $ const $ Branch1Node lvl opc () ()
    rn8 <- newNode $ const $ AppNode () () ()
    linkNodes (Ref rn4 1) (Ref rn6 1)
    linkNodes (Ref rn5 1) (Ref rn6 2)
    linkNodes (Ref rn4 2) (Ref rn7 0)
    linkNodes (Ref rn5 2) (Ref rn8 1)
    linkNodes (Ref rn7 1) (Ref rn8 0)
    linkNodes r0 $ Ref rn4 0
    linkNodes r1 $ Ref rn5 0
    linkNodes r2 $ Ref rn6 0
    linkNodes r3 $ Ref rn8 2
reduceNode n0@AppNode {} n1@Branch3PNode {} = reduceNode n1 n0
-- FFI Duplication
reduceNode (DupNode _ re _ r0 r1) (OperandNode op _)
    = copyIO1 r0 r1 OperandNode B.renameOp re op
reduceNode n0@OperandNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl re _ r0 r1) (OperandPNode opp _ r2)
    = copyIO2 lvl r0 r1 r2 OperandPNode (fmap . B.renameOp) re opp
reduceNode n0@OperandPNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl re _ r0 r1) (IOPNode iop _ r2)
    = copyIO2 lvl r0 r1 r2 IOPNode (fmap . B.renameInstr) re iop
reduceNode n0@IOPNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl re _ r0 r1) (IOContNode nbs _ r2)
    = shareIOCont lvl re nbs r0 r1 r2
reduceNode n0@IOContNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl re _ r0 r1) (Pure0Node _ r2)
    = commute1 (DupNode lvl re) Pure0Node r0 r1 r2
reduceNode n0@Pure0Node {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl re _ r0 r1) (Bind0Node _ r2)
    = commute1 (DupNode lvl re) Bind0Node r0 r1 r2
reduceNode n0@Bind0Node {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl re _ r0 r1) (Bind1Node nbs _ r2)
    = shareBind1 lvl re nbs r0 r1 r2
reduceNode n0@Bind1Node {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl0 re _ r0 r1) (Branch0Node lvl1 _ r2)
    = commute1 (DupNode lvl0 re) (Branch0Node lvl1) r0 r1 r2
reduceNode n0@Branch0Node {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl0 re _ r0 r1) (Branch1Node lvl1 op _ r2)
    = copyIO2 lvl0 r0 r1 r2 (Branch1Node lvl1) B.renameOp re op
reduceNode n0@Branch1Node {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl0 re _ r0 r1) (Branch2BNode lvl1 opc nbt _ r2 r3)
    = shareBranch2B lvl0 lvl1 re opc nbt r0 r1 r2 r3
reduceNode n0@Branch2BNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl0 re _ r0 r1) (Branch2PNode lvl1 op _ r2 r3)
    = commute2' (DupNode lvl0 re) (DupNode lvl0 re)
        (Branch2PNode lvl1 $ B.renameOp (M.map fst re) op)
        (Branch2PNode lvl1 $ B.renameOp (M.map snd re) op)
        r0 r1 r2 r3
reduceNode n0@Branch2PNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl0 re _ r0 r1) (Branch3PNode lvl1 op _ r2 r3)
    = commute2' (DupNode lvl0 re) (DupNode lvl0 re)
        (Branch3PNode lvl1 $ B.renameOp (M.map fst re) op)
        (Branch3PNode lvl1 $ B.renameOp (M.map snd re) op)
        r0 r1 r2 r3
reduceNode n0@Branch3PNode {} n1@DupNode {} = reduceNode n1 n0
-- FFI Dead
reduceNode (OperandNode _ _) (DeadNode _) = pure ()
reduceNode n0@DeadNode {} n1@OperandNode {} = reduceNode n1 n0
reduceNode (OperandPNode _ _ r0) (DeadNode _) = propagate1 r0 DeadNode
reduceNode n0@DeadNode {} n1@OperandPNode {} = reduceNode n1 n0
reduceNode (IOPNode _ _ r0) (DeadNode _) = propagate1 r0 DeadNode
reduceNode n0@DeadNode {} n1@IOPNode {} = reduceNode n1 n0
reduceNode (IOContNode _ _ r0) (DeadNode _) = propagate1 r0 DeadNode
reduceNode n0@DeadNode {} n1@IOContNode {} = reduceNode n1 n0
reduceNode (Pure0Node _ r0) (DeadNode _) = propagate1 r0 DeadNode
reduceNode n0@DeadNode {} n1@Pure0Node {} = reduceNode n1 n0
reduceNode (Bind0Node _ r0) (DeadNode _) = propagate1 r0 DeadNode
reduceNode n0@DeadNode {} n1@Bind0Node {} = reduceNode n1 n0
reduceNode (Bind1Node _ _ r0) (DeadNode _) = propagate1 r0 DeadNode
reduceNode n0@DeadNode {} n1@Bind1Node {} = reduceNode n1 n0
reduceNode (Branch0Node _ _ r0) (DeadNode _) = propagate1 r0 DeadNode
reduceNode n0@DeadNode {} n1@Branch0Node {} = reduceNode n1 n0
reduceNode (Branch1Node _ _ _ r0) (DeadNode _) = propagate1 r0 DeadNode
reduceNode n0@DeadNode {} n1@Branch1Node {} = reduceNode n1 n0
reduceNode (Branch2BNode _ _ _ _ r0 r1) (DeadNode _) = propagate2 r0 r1 DeadNode
reduceNode n0@DeadNode {} n1@Branch2BNode {} = reduceNode n1 n0
reduceNode (Branch2PNode _ _ _ r0 r1) (DeadNode _) = propagate2 r0 r1 DeadNode
reduceNode (Branch3PNode _ _ _ r0 r1) (DeadNode _) = propagate2 r0 r1 DeadNode
reduceNode n0@DeadNode {} n1@Branch2PNode {} = reduceNode n1 n0
-- FFI Book-keeping
reduceNode (RootNode name args _) (BoxNode _ _ r0)
    = propagate1 r0 $ RootNode name args
reduceNode n0@BoxNode {} n1@RootNode {} = reduceNode n1 n0
reduceNode (OperandNode op _) (BoxNode _ _ r0)
    = propagate1 r0 $ OperandNode op
reduceNode n0@BoxNode {} n1@OperandNode {} = reduceNode n1 n0
reduceNode (OperandPNode opp _ r0) (BoxNode lvl _ r1)
    = commute0 (OperandPNode opp) (BoxNode lvl) r0 r1
reduceNode n0@BoxNode {} n1@OperandPNode {} = reduceNode n1 n0
reduceNode (IONode b _) (BoxNode _ _ r0)
    = propagate1 r0 $ IONode b
reduceNode n0@BoxNode {} n1@IONode {} = reduceNode n1 n0
reduceNode (IOPNode iop _ r0) (BoxNode lvl _ r1)
    = commute0 (IOPNode iop) (BoxNode lvl) r0 r1
reduceNode n0@BoxNode {} n1@IOPNode {} = reduceNode n1 n0
reduceNode (IOContNode nbs _ r0) (BoxNode lvl _ r1)
    = commute0 (IOContNode nbs) (BoxNode lvl) r0 r1
reduceNode n0@BoxNode {} n1@IOContNode {} = reduceNode n1 n0
reduceNode (Pure0Node _ r0) (BoxNode lvl _ r1)
    = commute0 Pure0Node (BoxNode lvl) r0 r1
reduceNode n0@BoxNode {} n1@Pure0Node {} = reduceNode n1 n0
reduceNode (Bind0Node _ r0) (BoxNode lvl _ r1)
    = commute0 Bind0Node (BoxNode lvl) r0 r1
reduceNode n0@BoxNode {} n1@Bind0Node {} = reduceNode n1 n0
reduceNode (Bind1Node nbs _ r0) (BoxNode lvl _ r1)
    = commute0 (Bind1Node nbs) (BoxNode lvl) r0 r1
reduceNode n0@BoxNode {} n1@Bind1Node {} = reduceNode n1 n0
reduceNode (Branch0Node lvl0 _ r0) (BoxNode lvl1 _ r1) = commute0
    (Branch0Node $ if lvl0 < lvl1 then lvl0 else succ lvl0) (BoxNode lvl1) r0 r1
reduceNode n0@BoxNode {} n1@Branch0Node {} = reduceNode n1 n0
reduceNode (Branch1Node lvl0 op _ r0) (BoxNode lvl1 _ r1) = commute0
    (Branch1Node (if lvl0 < lvl1 then lvl0 else succ lvl0) op)
    (BoxNode lvl1)
    r0 r1
reduceNode n0@BoxNode {} n1@Branch1Node {} = reduceNode n1 n0
reduceNode (Branch2BNode lvl0 opc nbt _ r0 r1) (BoxNode lvl1 _ r2) = commute1
    (Branch2BNode (if lvl0 < lvl1 then lvl0 else succ lvl0) opc nbt)
    (BoxNode lvl1)
    r0 r1 r2
reduceNode n0@BoxNode {} n1@Branch2BNode {} = reduceNode n1 n0
reduceNode (Branch2PNode lvl0 opc _ r0 r1) (BoxNode lvl1 _ r2) = commute1
    (Branch2PNode (if lvl0 < lvl1 then lvl0 else succ lvl0) opc)
    (BoxNode lvl1)
    r0 r1 r2
reduceNode n0@BoxNode {} n1@Branch2PNode {} = reduceNode n1 n0
reduceNode (Branch3PNode lvl0 opc _ r0 r1) (BoxNode lvl1 _ r2) = commute1
    (Branch3PNode (if lvl0 < lvl1 then lvl0 else succ lvl0) opc)
    (BoxNode lvl1)
    r0 r1 r2
reduceNode n0@BoxNode {} n1@Branch3PNode {} = reduceNode n1 n0
reduceNode n0 n1 = error . show
    $ "unexpected node pairing" <> line <> pretty n0 <> line <> pretty n1
{-# INLINABLE reduceNode #-}

commute0
    :: HasRewriter sig m
    => (() -> () -> INetF ()) -> (() -> () -> INetF ())
    -> Ref -> Ref -> m ()
commute0 mk1 mk2 r0 r1 = do
    rn3 <- newNode $ const $ mk2 () ()
    rn4 <- newNode $ const $ mk1 () ()
    linkNodes (Ref rn3 1) (Ref rn4 1)
    linkNodes r0 $ Ref rn3 0
    linkNodes r1 $ Ref rn4 0
{-# INLINABLE commute0 #-}

commute1
    :: HasRewriter sig m
    => (() -> () -> () -> INetF ()) -> (() -> () -> INetF ())
    -> Ref -> Ref -> Ref -> m ()
commute1 mk1 mk2 = commute1' mk1 mk2 mk2
{-# INLINABLE commute1 #-}

commute1'
    :: HasRewriter sig m
    => (() -> () -> () -> INetF ())
    -> (() -> () -> INetF ()) -> (() -> () -> INetF ())
    -> Ref -> Ref -> Ref -> m ()
commute1' mk1 mk2a mk2b r0 r1 r2 = do
    rn3 <- newNode $ const $ mk2a () ()
    rn4 <- newNode $ const $ mk2b () ()
    rn5 <- newNode $ const $ mk1 () () ()
    linkNodes (Ref rn3 1) (Ref rn5 1)
    linkNodes (Ref rn4 1) (Ref rn5 2)
    linkNodes r0 $ Ref rn3 0
    linkNodes r1 $ Ref rn4 0
    linkNodes r2 $ Ref rn5 0
{-# INLINABLE commute1' #-}

commute2
    :: HasRewriter sig m
    => (() -> () -> () -> INetF ())
    -> (() -> () -> () -> INetF ())
    -> Ref -> Ref -> Ref -> Ref -> m ()
commute2 mk1 mk2 = commute2' mk1 mk1 mk2 mk2
{-# INLINABLE commute2 #-}

commute2'
    :: HasRewriter sig m
    => (() -> () -> () -> INetF ())
    -> (() -> () -> () -> INetF ())
    -> (() -> () -> () -> INetF ())
    -> (() -> () -> () -> INetF ())
    -> Ref -> Ref -> Ref -> Ref -> m ()
commute2' mk1a mk1b mk2a mk2b r0 r1 r2 r3 = do
    rn4 <- newNode $ const $ mk1a () () ()
    rn5 <- newNode $ const $ mk1b () () ()
    rn6 <- newNode $ const $ mk2a () () ()
    rn7 <- newNode $ const $ mk2b () () ()
    linkNodes (Ref rn4 1) (Ref rn6 1)
    linkNodes (Ref rn4 2) (Ref rn7 1)
    linkNodes (Ref rn5 1) (Ref rn6 2)
    linkNodes (Ref rn5 2) (Ref rn7 2)
    linkNodes r0 $ Ref rn6 0
    linkNodes r1 $ Ref rn7 0
    linkNodes r2 $ Ref rn4 0
    linkNodes r3 $ Ref rn5 0
{-# INLINABLE commute2' #-}

propagate1 :: HasRewriter sig m => Ref -> (() -> INetF ()) -> m ()
propagate1 r0 mk1 = do
    rn1 <- newNode $ const $ mk1 ()
    linkNodes r0 $ Ref rn1 0
{-# INLINABLE propagate1 #-}

propagate2 :: HasRewriter sig m => Ref -> Ref -> (() -> INetF ()) -> m ()
propagate2 r0 r1 mk1 = propagate1 r0 mk1 *> propagate1 r1 mk1
{-# INLINABLE propagate2 #-}

copyIO1
    :: HasRewriter sig m
    => Ref -> Ref -> (a -> () -> INetF ())
    -> (M.Map B.Name B.Operand -> a -> a) -> Renames -> a
    -> m ()
copyIO1 r0 r1 mk1 f re ffi = do
    propagate1 r0 $ mk1 $ f (M.map fst re) ffi
    propagate1 r1 $ mk1 $ f (M.map snd re) ffi
{-# INLINABLE copyIO1 #-}

copyIO2
    :: HasRewriter sig m
    => Level -> Ref -> Ref -> Ref -> (a -> () -> () -> INetF ())
    -> (M.Map B.Name B.Operand -> a -> a) -> Renames -> a
    -> m ()
copyIO2 lvl r0 r1 r2 mk1 f re ffi = commute1' (DupNode lvl re)
    (mk1 $ f (M.map fst re) ffi)
    (mk1 $ f (M.map snd re) ffi) r0 r1 r2
{-# INLINABLE copyIO2 #-}

copyIO3
    :: HasRewriter sig m
    => Level -> Ref -> Ref -> Ref -> Ref -> (a -> () -> () -> () -> INetF ())
    -> (M.Map B.Name B.Operand -> a -> a) -> Renames -> a
    -> m ()
copyIO3 lvl r0 r1 r2 r3 mk1 f re ffi = commute2'
    (DupNode lvl re) (DupNode lvl re)
    (mk1 $ f (M.map fst re) ffi)
    (mk1 $ f (M.map snd re) ffi)
    r0 r1 r2 r3
{-# INLINABLE copyIO3 #-}

shareIOCont
    :: HasRewriter sig m
    => Level -> Renames -> Preaction -> Ref -> Ref -> Ref -> m ()
shareIOCont = sharePreaction (<>) (pure pure) (pure pure) IOContNode
{-# INLINABLE shareIOCont #-}

shareBind1
    :: HasRewriter sig m
    => Level -> Renames -> Preaction -> Ref -> Ref -> Ref -> m ()
shareBind1 lvl = sharePreaction (const id) (wrap fst) (wrap snd) Bind1Node lvl
  where
    wrap sel re r0 = do
        rn1 <- newNode $ const $ DupNode lvl re () () ()
        rn2 <- newNode $ const $ DeadNode ()
        linkNodes (Ref rn1 $ sel (2, 1)) (Ref rn2 0)
        linkNodes r0 $ Ref rn1 $ sel (1, 2)
        pure $ Ref rn1 0
{-# INLINABLE shareBind1 #-}

shareBranch2B
    :: HasRewriter sig m
    => Level -> Level -> Renames -> B.Operand -> Preaction
    -> Ref -> Ref -> Ref -> Ref -> m ()
shareBranch2B lvl0 lvl1 re opc nbt r0 r1 r2 r3 = case sharePreaction' nbt of
    Nothing -> copyIO3 lvl0 r0 r1 r2 r3 mk renamePreaction re nbt
    Just (name, sb) | null (B.bodyInstrs sb) -> do
        let mkPreaction rn = SimplePreaction (mkName rn) sb
        rn4 <- newNode $ \rn4 -> mk (mkPreaction rn4) () () ()
        rn5 <- newNode $ \rn5 -> mk (mkPreaction rn5) () () ()
        let re' = M.singleton name (mkRef t rn4, mkRef t rn5)
            t = B.instrType $ B.bodyTerm sb
        rn6 <- newNode $ const $ DupNode lvl0 re () () ()
        rn7 <- newNode $ const $ DupNode lvl0 re () () ()
        r8 <- w1 re' $ Ref rn4 0
        r9 <- w2 re' $ Ref rn5 0
        linkNodes (Ref rn4 1) (Ref rn6 1)
        linkNodes (Ref rn5 1) (Ref rn6 2)
        linkNodes (Ref rn4 2) (Ref rn7 1)
        linkNodes (Ref rn5 2) (Ref rn7 2)
        linkNodes r0 r8
        linkNodes r1 r9
        linkNodes r2 $ Ref rn6 0
        linkNodes r3 $ Ref rn7 0
    Just (name, sb) -> do
        let names = preactionNames nbt
            extraArgs = nub $ sb ^.. B.bodyFreeRefs
            args = M.assocs re
            args' = uncurry (B.:=)
                <$> (swap <$> extraArgs)
                <> (second (B.opType . fst) <$> args)
        rn4 <- newNode $ \rn4 -> RootNode (mkName rn4) args' ()
        let mkPreact sel rn = mkCall sel rn
            mkCall sel rn = SimplePreaction (mkName rn) $ B.Body mempty
                $ B.Call ltt (mkName rn4)
                $ (<>) (uncurry B.Reference <$> extraArgs)
                $ sel . snd <$> args
            sb' = B.concatBody name sb $ B.Body mempty $ B.Pure sop
            (sop, gop, ltt) = case names of
                [] -> (B.Empty, const $ const B.Empty, B.IntType 0)
                [name'] -> (shareRef name', const id, snd name')
                _ -> (B.Tuple $ shareRef <$> names, mkGetElement
                    , B.TupleType $ snd <$> names)
        rn5 <- newNode $ const $ IONode sb' ()
        rn6 <- newNode $ \rn6 -> mk (mkPreact fst rn6) () () ()
        rn7 <- newNode $ \rn7 -> mk (mkPreact snd rn7) () () ()
        let re' = M.fromList $ zipWith (dupNames gop ltt rn5 rn6) [0..] names
        rn8 <- newNode $ const $ DupNode lvl0 re () () ()
        rn9 <- newNode $ const $ DupNode lvl0 re () () ()
        r10 <- w1 re' (Ref rn6 0)
        r11 <- w2 re' (Ref rn7 0)
        linkNodes (Ref rn4 0) (Ref rn5 0)
        linkNodes (Ref rn6 1) (Ref rn8 1)
        linkNodes (Ref rn7 1) (Ref rn8 2)
        linkNodes (Ref rn6 2) (Ref rn9 1)
        linkNodes (Ref rn7 2) (Ref rn9 2)
        linkNodes r0 r10
        linkNodes r1 r11
        linkNodes r2 $ Ref rn8 0
        linkNodes r3 $ Ref rn9 0
  where
    mk = Branch2BNode lvl1 opc
    w1 = wrap fst
    w2 = wrap snd

    wrap sel re' r4 = do
        rn5 <- newNode $ const $ DupNode lvl0 re' () () ()
        rn6 <- newNode $ const $ DeadNode ()
        linkNodes (Ref rn5 $ sel (2, 1)) (Ref rn6 0)
        linkNodes r4 $ Ref rn5 $ sel (1, 2)
        pure $ Ref rn5 0

    dupNames gop ltt rn3 rn4 idx (name, _)
        = (name, (gop idx $ mkRef ltt rn3, gop idx $ mkRef ltt rn4))

    shareRef :: (B.Name, B.Type) -> B.Operand
    shareRef (name, t) = B.Reference t name
{-# INLINABLE shareBranch2B #-}

sharePreaction
    :: HasRewriter sig m
    => (Renames -> Renames -> Renames)
    -> (Renames -> Ref -> m Ref) -> (Renames -> Ref -> m Ref)
    -> (Preaction -> () -> () -> INetF ())
    -> Level -> Renames -> Preaction -> Ref -> Ref -> Ref -> m ()
sharePreaction append w1 w2 mk lvl re nbs r0 r1 r2 = case sharePreaction' nbs of
    Nothing -> copyIO2 lvl r0 r1 r2 mk renamePreaction re nbs
    Just (name, sb) | null (B.bodyInstrs sb) -> do
        let mkPreaction rn = SimplePreaction (mkName rn) sb
        rn3 <- newNode $ \rn3 -> mk (mkPreaction rn3) () ()
        rn4 <- newNode $ \rn4 -> mk (mkPreaction rn4) () ()
        let re' = M.singleton name (mkRef t rn3, mkRef t rn4)
            t = B.instrType $ B.bodyTerm sb
        rn5 <- newNode $ const $ DupNode lvl (append re' re) () () ()
        r6 <- w1 re' $ Ref rn3 0
        r7 <- w2 re' $ Ref rn4 0
        linkNodes (Ref rn3 1) (Ref rn5 1)
        linkNodes (Ref rn4 1) (Ref rn5 2)
        linkNodes r0 r6
        linkNodes r1 r7
        linkNodes r2 $ Ref rn5 0
    Just (name, sb) -> do
        let names = preactionNames nbs
            extraArgs = nub $ sb ^.. B.bodyFreeRefs
            args = M.assocs re
            args' = uncurry (B.:=)
                <$> (swap <$> extraArgs)
                <> (second (B.opType . fst) <$> args)
        rn3 <- newNode $ \rn3 -> RootNode (mkName rn3) args' ()
        let mkPreact sel rn = mkCall sel rn
            mkCall sel rn = SimplePreaction (mkName rn) $ B.Body mempty
                $ B.Call ltt (mkName rn3)
                $ (<>) (uncurry B.Reference <$> extraArgs)
                $ sel . snd <$> args
            sb' = B.concatBody name sb $ B.Body mempty $ B.Pure sop
            (sop, gop, ltt) = case names of
                [] -> (B.Empty, const $ const B.Empty, B.IntType 0)
                [name'] -> (shareRef name', const id, snd name')
                _ -> (B.Tuple $ shareRef <$> names, mkGetElement
                    , B.TupleType $ snd <$> names)
        rn4 <- newNode $ const $ IONode sb' ()
        rn5 <- newNode $ \rn5 -> mk (mkPreact fst rn5) () ()
        rn6 <- newNode $ \rn6 -> mk (mkPreact snd rn6) () ()
        let re' = M.fromList $ zipWith (dupNames gop ltt rn5 rn6) [0..] names
        rn7 <- newNode $ const $ DupNode lvl (append re' re) () () ()
        r8 <- w1 re' (Ref rn5 0)
        r9 <- w2 re' (Ref rn6 0)
        linkNodes (Ref rn3 0) (Ref rn4 0)
        linkNodes (Ref rn5 1) (Ref rn7 1)
        linkNodes (Ref rn6 1) (Ref rn7 2)
        linkNodes r0 r8
        linkNodes r1 r9
        linkNodes r2 $ Ref rn7 0
  where
    dupNames gop ltt rn3 rn4 idx (name, _)
        = (name, (gop idx $ mkRef ltt rn3, gop idx $ mkRef ltt rn4))

    shareRef :: (B.Name, B.Type) -> B.Operand
    shareRef (name, t) = B.Reference t name
{-# INLINABLE sharePreaction #-}

sharePreaction' :: Preaction -> Maybe (B.Name, B.Body)
sharePreaction' (SimplePreaction name b) = pure (name, b)
sharePreaction' (BranchPreaction opc nb1 nb2) = pure (B.UnusedName
    , B.Body mempty $ B.Branch opc (applyCont nb1 eb) (applyCont nb2 eb))
  where
    eb = B.Body mempty $ B.Pure B.Empty
sharePreaction' (ConcatPreaction nb1 nb2)
    = case (sharePreaction' nb1, sharePreaction' nb2) of
        (Nothing, Nothing) -> Nothing
        (Just (name1, b1), Nothing) -> Just (name1, b1)
        (Nothing, Just (name2, b2)) -> Just (name2, b2)
        (Just (name1, b1), Just (name2, b2))
            -> Just (name2, B.concatBody name1 b1 b2)
sharePreaction' EmptyPreaction = Nothing
{-# INLINABLE sharePreaction' #-}

preactionNames :: Preaction -> [(B.Name, B.Type)]
preactionNames (SimplePreaction name b) = case name of
    B.UnusedName -> []
    _ -> case B.instrType $ B.bodyTerm b of
        B.IntType 0 -> []
        t -> [(name, t)]
preactionNames (BranchPreaction _ nb1 nb2)
    = preactionNames nb1 <> preactionNames nb2
preactionNames (ConcatPreaction nb1 nb2)
    = preactionNames nb1 <> preactionNames nb2
preactionNames EmptyPreaction = []
{-# INLINABLE preactionNames #-}

mkLambda :: HasRewriter sig m => Ref -> (() -> () -> INetF ()) -> m ()
mkLambda r0 mk1 = do
    rn1 <- newNode $ const $ mk1 () ()
    rn2 <- newNode $ const $ LamNode () () ()
    linkNodes (Ref rn1 0) (Ref rn2 1)
    linkNodes (Ref rn1 1) (Ref rn2 2)
    linkNodes r0 $ Ref rn2 0
{-# INLINE mkLambda #-}

applyCont :: Preaction -> B.Body -> B.Body
applyCont nbs = case nbs of
    SimplePreaction name b -> B.concatBody name b
    BranchPreaction op nb1 nb2 -> B.concatBody B.UnusedName
        $ B.Body mempty $ B.Branch op (applyCont nb1 eb) (applyCont nb2 eb)
    ConcatPreaction nb1 nb2 -> applyCont nb1 . applyCont nb2
    EmptyPreaction -> id
  where
    eb = B.Body mempty $ B.Pure B.Empty
{-# INLINABLE applyCont #-}

renamePreaction :: M.Map B.Name B.Operand -> Preaction -> Preaction
renamePreaction re nbs = case nbs of
    SimplePreaction name b
        -> SimplePreaction name (B.renameBody re b)
    BranchPreaction opc bt bf -> BranchPreaction
        (B.renameOp re opc) (renamePreaction re bt) (renamePreaction re bf)
    ConcatPreaction b1 b2
        -> ConcatPreaction (renamePreaction re b1) (renamePreaction re b2)
    EmptyPreaction -> EmptyPreaction
{-# INLINABLE renamePreaction #-}

mkName :: Int -> B.Name
mkName = B.Name . fromIntegral
{-# INLINE mkName #-}

mkRef :: B.Type -> Int -> B.Operand
mkRef t = B.Reference t . mkName
{-# INLINE mkRef #-}

mkSelect :: B.Operand -> B.Operand -> B.Operand -> B.Operand
mkSelect opc (B.Constant B.B1) (B.Constant B.B0) = opc
mkSelect opc opt opf
    | opt == opf = opt
    | otherwise = B.Select opc opt opf
{-# INLINABLE mkSelect #-}

mkGetElement :: Int -> B.Operand -> B.Operand
mkGetElement idx (B.Tuple ops) = ops !! idx
mkGetElement idx opt = B.GetElement idx opt
{-# INLINABLE mkGetElement #-}

type HasRewriter sig m = (Has (State INet) sig m, Has (State INetPairs) sig m
    , Has (State INetSize) sig m)

newtype INetSize = INetSize { unINetSize :: Int }
    deriving newtype (Enum, Eq, Num, Ord)

_INetSize :: Iso' INetSize Int
_INetSize = iso unINetSize INetSize
{-# INLINE _INetSize #-}

-- TODO: See if removing the self-reference from the constructor is possible.
newNode :: HasRewriter sig m => (Int -> INetF ()) -> m Int
newNode mk = do
    idx <- newNodeIndex
    modify $ _INet . at idx ?~ (unsetRef <$ mk idx)
    pure idx
  where
    unsetRef = Ref (-1) (-1)
{-# INLINE newNode #-}

newNodeIndex :: Has (State INetSize) sig m => m Int
newNodeIndex = gets unINetSize <* modify @INetSize succ
{-# INLINE newNodeIndex #-}

linkNodes :: HasRewriter sig m => Ref -> Ref -> m ()
linkNodes r0 r1 = do
    net <- get
    modify @INet $ ix (refNode r0) . ix (refPort r0) .~ r1
    modify @INet $ ix (refNode r1) . ix (refPort r1) .~ r0
    modify $ _INetPairs %~ updatePairs net
  where
    updatePairs net
        | refPort r0 == 0 && refPort r1 == 0
            = IS.insert (refNode r0) . IS.insert (refNode r1)
        | refPort r0 == 0 = IS.delete (refNode r0) . IS.delete (deref r0)
        | refPort r1 == 0 = IS.delete (refNode r1) . IS.delete (deref r1)
        | otherwise = id
      where
        deref r2 = fromMaybe (-1) $ unINet net IM.!? refNode r2
            >>= (^? folded . to refNode)
{-# INLINE linkNodes #-}

data TraceRewrite m a where
    TraceRewrite
        :: Ref -> Ref -> INetF Ref -> INetF Ref -> m r -> TraceRewrite m r

traceRewrite
    :: Has TraceRewrite sig m
    => Ref -> Ref -> INetF Ref -> INetF Ref -> m r -> m r
traceRewrite r0 r1 n0 n1 cont = send $ TraceRewrite r0 r1 n0 n1 cont
{-# INLINE traceRewrite #-}

