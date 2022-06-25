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

    The easiest way of using the evaluator is to use 'reduce'.

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
    , HasRewriter
    , compileINet
    , reduce
    , propagate1
    , propagate2
    , mkLambda
    , mkChurchBool
    , newNode
    , newName
    , newLabel
    , linkNodes
    -- * Debugging
    , TraceRewrite(..)
    , traceRewrite
    , deleteBoxes
    ) where

import Control.Algebra (Has, send)
import Control.Carrier.Writer.Church (Writer, execWriter, tell)
import Control.Effect.State (State, get, gets, modify)
import Control.Lens.At (At(at), Index, Ixed(ix), IxValue)
import Control.Lens.Cons (_head)
import Control.Lens.Fold (IndexedFold, filtered, folded, imapMOf_, (^?))
import Control.Lens.Getter (to)
import Control.Lens.Indexed (Indexed(Indexed), indexing)
import Control.Lens.Iso (Iso', iso)
import Control.Lens.Setter ((.~), (%~), (?~))
import Control.Lens.Traversal (traversed)
import Control.Lens.Wrapped (_Wrapped)
import Data.DList (snoc)
import Data.Foldable (find)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Maybe (fromMaybe)
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

{-|
    A set of interacting nodes in an interaction net.

    This data structure must be kept in sync with its 'INet', else weird things
    will happen.
-}
newtype INetPairs = INetPairs { unINetPairs :: IS.IntSet }
    deriving newtype (Semigroup, Monoid)

_INetPairs :: Iso' INetPairs IS.IntSet
_INetPairs = iso unINetPairs INetPairs
{-# INLINE _INetPairs #-}

-- | A reference to a port in an interaction net.
data Ref = Ref
    { refNode :: Int
    -- ^ The index of the port's node.
    , refPort :: Int
    -- ^ The index of the port within the node's port list.
    } deriving stock (Eq, Ord, Show)

instance Pretty Ref where
    pretty r0 = pretty (refNode r0) <> ":" <> pretty (refPort r0)

{-|
    The "level" of a node.

    'DupNode'-related interactions use this to determine whether two nodes
    annihilate (same level) or commute (different levels).
-}
newtype Level = Level { unLevel :: Int }
    deriving newtype (Enum, Eq, Ord, Num, Pretty)

{-|
    A node in the interaction net.

    The documentation for each constructor indicates the intended type for the
    node's ports.

    +----------+--------------------------------------------------------+
    | Type     | Description                                            |
    +==========+========================================================+
    | @a -> b@ | A function taking an @a@ as input and returning a @b@. |
    +----------+--------------------------------------------------------+
    | @i{n}@   | An operand of the specified type (e.g. @i8@).          |
    +----------+--------------------------------------------------------+
    | @IO a@   | An IO action returning a value of type @a@.            |
    +----------+--------------------------------------------------------+
    | @B@      | A list of blocks.                                      |
    +----------+--------------------------------------------------------+
    | @CB@     | A fully-reduced (i.e. completed) list of blocks.       |
    +----------+--------------------------------------------------------+
    | @NB@     | A list of named blocks.                                |
    +----------+--------------------------------------------------------+
    | @T@      | A tunnel for pairing 'DupNode' when sharing blocks.    |
    +----------+--------------------------------------------------------+
-}
data INetF a
    -- | (a -> b, a, b)
    = AppNode a a a
    -- | (a -> b, a, b)
    | LamNode a a a
    -- | (a, a, a)
    | DupNode Level a a a
    -- | a
    | DeadNode a
    -- | (a, a) and the non-principal node is in a new box.
    | BoxNode Level a a
    -- FFI
    -- | CB
    | ExternalRootNode B.ForeignName [B.Named B.Type] B.Type a
    -- | CB
    | PrivateRootNode B.Name B.Name a
    -- | (CB, B)
    | AccumIONode B.IBlock a a
    -- | (NB, CB)
    | AccumNBNode B.BlockList a a
    -- | i{n}
    | OperandNode B.Operand a
    -- | {... ->} i{n}
    | OperandANode (B.Partial B.Operand) a
    -- | (i{m}, {... ->} i{n})
    | OperandPNode (B.Partial B.Operand) a a
    -- | B
    | IONode B.BlockList a
    -- | {... ->} IO i{n}
    | IOANode (B.Partial B.Instruction) a
    -- | (i{m}, {... ->} IO i{n})
    | IOPNode (B.Partial B.Instruction) a a
    -- | (IO a, (a -> B) -> B)
    | IOPureNode a a
    -- | IO a
    | IOContNode B.Instruction a
    -- | (B, i{n})
    | ReturnCNode a a
    -- | (i{n}, B)
    | ReturnFNode a a
    -- | (IO a, (a -> IO b) -> IO b)
    | Bind0BNode a a
    -- | (IO a, a -> B, B)
    | Bind0CNode a a a
    -- | (IO a, a -> B, B)
    | Bind0FNode B.Name a a a
    -- | (B, IO a, (a -> B) -> B)
    | Bind1CNode a a a
    -- | (B, B)
    | Bind1FNode (B.Named B.Instruction) a a
    -- | (B, i{n}, B, B)
    | Branch0CNode a a a a
    -- | (i{n}, B, B, B)
    | Branch0FNode a a a a
    -- | (CB, NB)
    | LabelNode B.Label a a
    -- | NB
    | NamedBlockNode B.NamedBlockList a
    -- | (NB, NB, NB)
    | Merge0Node a a a
    -- | (NB, NB)
    | Merge1Node B.NamedBlockList a a
    -- | (a, T, T, a)
    | TBuildNode Level BuildType B.Name a a a a
    -- | (B, T)
    | TEntryNode B.Name B.Operand a a
    -- | (T, T, T)
    | TSplitNode a a a
    -- | T
    | TCloseNode a
    -- | (T, a, a)
    | TLeaveNode BuildType a a a
    -- | (T, T, T, i1)
    | TMatchNode Level a a a a
    -- | (a, i{n} -> a, i{n})
    | PArgumentNode a a a
    -- | (a, a)
    | PReduceNode a a
    deriving stock (Foldable, Functor, Traversable)

instance Pretty a => Pretty (INetF a) where
    pretty (AppNode r0 r1 r2) = "App" <+> pretty r0 <+> pretty r1 <+> pretty r2
    pretty (LamNode r0 r1 r2) = "Lam" <+> pretty r0 <+> pretty r1 <+> pretty r2
    pretty (DupNode lvl r0 r1 r2)
        = "Dup" <+> pretty lvl <+> pretty r0 <+> pretty r1 <+> pretty r2
    pretty (DeadNode r0) = "Dead" <+> pretty r0
    pretty (BoxNode lvl r0 r1)
        = "Box" <+> pretty lvl <+> pretty r0 <+> pretty r1
    pretty (ExternalRootNode fname args ret r0)
        = "ExternalRoot" <+> pretty r0
        <+> pretty fname <+> tupled (pretty <$> args) <+> pretty ret
    pretty (PrivateRootNode fname namep r0)
        = "PrivateRoot" <+> pretty r0 <+> pretty fname <+> pretty namep
    pretty (AccumIONode ib r0 r1)
        = "AccumIO" <+> pretty r0 <+> pretty r1 <> nest 4 (line <> pretty ib)
    pretty (AccumNBNode bs r0 r1)
        = "AccumNB" <+> pretty r0 <+> pretty r1 <> nest 4 (line <> pretty bs)
        <> nest 4 (line <> pretty bs)
    pretty (OperandNode op r0) = "Operand" <+> pretty r0 <+> pretty op
    pretty (OperandANode opp r0) = "OperandA" <+> pretty r0 <+> pretty opp
    pretty (OperandPNode opp r0 r1)
        = "OperandP" <+> pretty r0 <+> pretty r1 <+> pretty opp
    pretty (IONode bs r0) = "IO" <+> pretty r0 <> nest 4 (line <> pretty bs)
    pretty (IOANode iop r0) = "IOA" <+> pretty r0 <+> pretty iop
    pretty (IOPNode iop r0 r1)
        = "IOP" <+> pretty r0 <+> pretty r1 <+> pretty iop
    pretty (IOPureNode r0 r1) = "IOPure" <+> pretty r0 <+> pretty r1
    pretty (IOContNode instr r0)
        = "IOCont" <+> pretty r0 <> nest 4 (line <> pretty instr)
    pretty (ReturnCNode r0 r1) = "ReturnC" <+> pretty r0 <+> pretty r1
    pretty (ReturnFNode r0 r1) = "ReturnF" <+> pretty r0 <+> pretty r1
    pretty (Bind0BNode r0 r1) = "Bind0B" <+> pretty r0 <+> pretty r1
    pretty (Bind0CNode r0 r1 r2)
        = "Bind0C" <+> pretty r0 <+> pretty r1 <+> pretty r2
    pretty (Bind0FNode name r0 r1 r2)
        = "Bind0F" <+> pretty r0 <+> pretty r1 <+> pretty r2 <+> pretty name
    pretty (Bind1CNode r0 r1 r2)
        = "Bind1C" <+> pretty r0 <+> pretty r1 <+> pretty r2
    pretty (Bind1FNode instr r0 r1)
        = "Bind1F" <+> pretty r0 <+> pretty r1 <> nest 4 (line <> pretty instr)
    pretty (Branch0CNode r0 r1 r2 r3)
        = "Branch0C" <+> pretty r0 <+> pretty r1 <+> pretty r2 <+> pretty r3
    pretty (Branch0FNode r0 r1 r2 r3)
        = "Branch0F" <+> pretty r0 <+> pretty r1 <+> pretty r2 <+> pretty r3
    pretty (LabelNode lbl r0 r1)
        = "Label" <+> pretty r0 <+> pretty r1 <+> pretty lbl
    pretty (NamedBlockNode nbs r0)
        = "NamedBlock" <+> pretty r0 <> nest 4 (line <> pretty nbs)
    pretty (Merge0Node r0 r1 r2)
        = "Merge0" <+> pretty r0 <+> pretty r1 <+> pretty r2
    pretty (Merge1Node nbs r0 r1)
        = "Merge1" <+> pretty r0 <+> pretty r1 <> nest 4 (line <> pretty nbs)
    pretty (TBuildNode lvl t namep r0 r1 r2 r3) = "TBuild"
        <+> pretty lvl <+> pretty r0 <+> pretty r1 <+> pretty r2 <+> pretty r3
        <+> pretty t <+> pretty namep
    pretty (TEntryNode name opp r0 r1)
        = "TEntry" <+> pretty r0 <+> pretty r1 <+> pretty name <+> pretty opp
    pretty (TSplitNode r0 r1 r2)
        = "TSplit" <+> pretty r0 <+> pretty r1 <+> pretty r2
    pretty (TCloseNode r0) = "TClose" <+> pretty r0
    pretty (TLeaveNode t r0 r1 r2)
        = "TLeave" <+> pretty r0 <+> pretty r1 <+> pretty r2 <+> pretty t
    pretty (TMatchNode lvl r0 r1 r2 r3) = "TMatch"
        <+> pretty lvl <+> pretty r0 <+> pretty r1 <+> pretty r2 <+> pretty r3
    pretty (PArgumentNode r0 r1 r2)
        = "PArgument" <+> pretty r0 <+> pretty r1 <+> pretty r2
    pretty (PReduceNode r0 r1) = "PReduce" <+> pretty r0 <+> pretty r1

instance Ixed (INetF a) where
    ix idx f = indexing traverse $ Indexed go
      where
        go idx' x
          | idx == idx' = f x
          | otherwise = pure x
    {-# INLINE ix #-}

type instance Index (INetF a) = Int
type instance IxValue (INetF a) = a

data BuildType = BuildOperand | BuildIO
    deriving stock (Eq, Ord, Show, Read)

instance Pretty BuildType where
    pretty BuildOperand = "Operand"
    pretty BuildIO = "IO"

-- | Compiles an interaction net and a list of externals into a backend program.
compileINet
    :: (HasRewriter sig m, Has TraceRewrite sig m)
    => [B.ForeignNamed B.External] -> m B.Program
compileINet exts = B.Program exts <$> reduce
{-# INLINABLE compileINet #-}

-- | Reduces the interaction net into a list of functions.
reduce :: (HasRewriter sig m, Has TraceRewrite sig m) => m [B.NamedFunction]
reduce = execWriter $ try *> lintFinal
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
        , Has (Writer [B.NamedFunction]) sig m)
        => m ()
    try = do
        net <- get
        case net ^? _INetPairs . _Wrapped . _head of
            Nothing -> pure ()
            Just rn0 -> go $ Ref rn0 0

    isRoot :: INetF Ref -> Bool
    isRoot ExternalRootNode {} = True
    isRoot PrivateRootNode {} = True
    isRoot TEntryNode {} = True
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
            Just _ -> do
                deleteBoxes
                net' <- get @INet
                error . show $ "lint: failed to reduce root"
                    <> line <> pretty net'
{-# INLINABLE reduce #-}

reduceNode
    :: (HasRewriter sig m, Has (Writer [B.NamedFunction]) sig m)
    => INetF Ref -> INetF Ref -> m ()
reduceNode (AppNode _ r0 r1) (LamNode _ r2 r3) = do
    rn4 <- newNode $ BoxNode 0 () ()
    rn5 <- newNode $ BoxNode 0 () ()
    linkNodes r0 $ Ref rn4 0
    linkNodes r1 $ Ref rn5 0
    linkNodes r2 $ Ref rn4 1
    linkNodes r3 $ Ref rn5 1
reduceNode n0@LamNode {} n1@AppNode {} = reduceNode n1 n0
reduceNode (AppNode _ r0 r1) (DupNode lvl _ r2 r3)
    = commute2 AppNode (DupNode lvl) r0 r1 r2 r3
reduceNode n0@DupNode {} n1@AppNode {} = reduceNode n1 n0
reduceNode (LamNode _ r0 r1) (DupNode lvl _ r2 r3)
    = commute2 LamNode (DupNode $ succ lvl) r0 r1 r2 r3
reduceNode n0@DupNode {} n1@LamNode {} = reduceNode n1 n0
reduceNode (DupNode lvl1 _ r0 r1) (DupNode lvl2 _ r2 r3)
    | lvl1 == lvl2 = linkNodes r0 r2 *> linkNodes r1 r3
    | otherwise = commute2 (DupNode lvl1) (DupNode lvl2) r0 r1 r2 r3
reduceNode (AppNode _ r0 r1) (DeadNode _) = propagate2 r0 r1 DeadNode
reduceNode n0@DeadNode {} n1@AppNode {} = reduceNode n1 n0
reduceNode (LamNode _ r0 r1) (DeadNode _) = propagate2 r0 r1 DeadNode
reduceNode n0@DeadNode {} n1@LamNode {} = reduceNode n1 n0
reduceNode (DupNode _ _ r0 r1) (DeadNode _) = propagate2 r0 r1 DeadNode
reduceNode n0@DeadNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DeadNode _) (DeadNode _) = pure ()
-- Book-keeping
reduceNode (AppNode _ r0 r1) (BoxNode lvl _ r2)
    = commute1 AppNode (BoxNode lvl) r0 r1 r2
reduceNode n0@BoxNode {} n1@AppNode {} = reduceNode n1 n0
reduceNode (LamNode _ r0 r1) (BoxNode lvl _ r2)
    = commute1 LamNode (BoxNode $ succ lvl) r0 r1 r2
reduceNode n0@BoxNode {} n1@LamNode {} = reduceNode n1 n0
reduceNode (DupNode lvl0 _ r0 r1) (BoxNode lvl1 _ r2) = commute1
    (DupNode $ if lvl0 < lvl1 then lvl0 else succ lvl0)
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
reduceNode (ExternalRootNode fname args ret _) (IONode b _)
    = tell @[B.NamedFunction] $ pure $ Right
    $ B.ExternalName fname B.:= B.Function args ret b
reduceNode n0@IONode {} n1@ExternalRootNode {} = reduceNode n1 n0
reduceNode (PrivateRootNode name namep _) (IONode bs _)
    = tell @[B.NamedFunction] $ pure $ Left
    $ name B.:= B.ImplicitFunction [namep B.:= B.IntType 1] bs
reduceNode n0@IONode {} n1@PrivateRootNode {} = reduceNode n1 n0
reduceNode (AccumIONode ib _ r0) (IONode bs _) = propagate1 r0
    $ IONode $ B._entryBlock . B._blockInstrs %~ (B.unIBlock ib <>) $ bs
reduceNode n0@IONode {} n1@AccumIONode {} = reduceNode n1 n0
reduceNode (AccumIONode ib _ r0) (ReturnCNode _ r1) = do
    rn2 <- newNode $ AccumIONode ib () ()
    rn3 <- newNode $ ReturnFNode () ()
    rn4 <- newNode $ PReduceNode () ()
    linkNodes (Ref rn2 0) (Ref rn3 1)
    linkNodes (Ref rn3 0) (Ref rn4 1)
    linkNodes r0 $ Ref rn2 1
    linkNodes r1 $ Ref rn4 0
reduceNode n0@ReturnCNode {} n1@AccumIONode {} = reduceNode n1 n0
reduceNode (AccumIONode ib _ r0) (Bind1CNode _ r1 r2) = do
    name <- newName
    rn3 <- newNode $ AccumIONode ib () ()
    rn4 <- newNode $ Bind0FNode name () () ()
    rn5 <- newNode $ PReduceNode () ()
    linkNodes (Ref rn3 0) (Ref rn4 2)
    linkNodes (Ref rn4 0) (Ref rn5 1)
    linkNodes r0 $ Ref rn3 1
    linkNodes r1 $ Ref rn5 0
    linkNodes r2 $ Ref rn4 1
reduceNode n0@Bind1CNode {} n1@AccumIONode {} = reduceNode n1 n0
reduceNode (AccumIONode ib _ r0) (Bind1FNode instr _ r1) = do
    let ib' = B._IBlock %~ (`snoc` instr) $ ib
    rn2 <- newNode $ AccumIONode ib' () ()
    linkNodes r0 $ Ref rn2 1
    linkNodes r1 $ Ref rn2 0
reduceNode n0@Bind1FNode {} n1@AccumIONode {} = reduceNode n1 n0
reduceNode (AccumIONode ib _ r0) (Branch0CNode _ r1 r2 r3) = do
    rn4 <- newNode $ AccumIONode ib () ()
    rn5 <- newNode $ Branch0FNode () () () ()
    rn6 <- newNode $ PReduceNode () ()
    linkNodes (Ref rn4 0) (Ref rn5 3)
    linkNodes (Ref rn5 0) (Ref rn6 1)
    linkNodes r0 $ Ref rn4 1
    linkNodes r1 $ Ref rn6 0
    linkNodes r2 $ Ref rn5 1
    linkNodes r3 $ Ref rn5 2
reduceNode n0@Branch0CNode {} n1@AccumIONode {} = reduceNode n1 n0
reduceNode (AccumNBNode bs _ r0) (NamedBlockNode nbs _)
    = propagate1 r0 $ IONode $ B._namedBlocks %~ (<>) nbs $ bs
reduceNode n0@NamedBlockNode {} n1@AccumNBNode {} = reduceNode n1 n0
reduceNode (OperandANode opp _) (AppNode _ r0 r1) = do
    rn2 <- newNode $ PArgumentNode () () ()
    propagate1 (Ref rn2 1) $ OperandANode opp
    linkNodes r0 $ Ref rn2 2
    linkNodes r1 $ Ref rn2 0
reduceNode n0@AppNode {} n1@OperandANode {} = reduceNode n1 n0
reduceNode (OperandPNode opp _ r0) (OperandNode op _)
    = case B.addOperand op opp of
        Left opp' -> mkLambda r0 $ OperandPNode opp'
        Right op' -> propagate1 r0 $ OperandNode op'
reduceNode n0@OperandNode {} n1@OperandPNode {} = reduceNode n1 n0
reduceNode (IOANode iop _) (AppNode _ r0 r1) = do
    rn2 <- newNode $ PArgumentNode () () ()
    propagate1 (Ref rn2 1) $ IOANode iop
    linkNodes r0 $ Ref rn2 2
    linkNodes r1 $ Ref rn2 0
reduceNode n0@AppNode {} n1@IOANode {} = reduceNode n1 n0
reduceNode (IOPNode iop _ r0) (OperandNode op _)
    = case B.addOperand op iop of
        Left iop' -> mkLambda r0 $ IOPNode iop'
        Right instr -> propagate1 r0 $ IOContNode instr
reduceNode n0@OperandNode {} n1@IOPNode {} = reduceNode n1 n0
reduceNode (ReturnFNode _ r0) (OperandNode op _) = propagate1 r0
    $ IONode $ B.BlockList (B.Block mempty $ B.Return op) mempty
reduceNode n0@OperandNode {} n1@ReturnFNode {} = reduceNode n1 n0
reduceNode (Bind0BNode _ r0) (IOPureNode _ r1) = reassocPure r0 r1
reduceNode n0@IOPureNode {} n1@Bind0BNode {} = reduceNode n1 n0
reduceNode (Bind0BNode _ r0) (IOContNode instr _) = do
    rn1 <- newNode $ IOContNode instr ()
    reassocCont r0 $ Ref rn1 0
reduceNode n0@IOContNode {} n1@Bind0BNode {} = reduceNode n1 n0
reduceNode (Bind0BNode _ r0) (PArgumentNode _ r1 r2) = do
    rn3 <- newNode $ PArgumentNode () () ()
    linkNodes r1 $ Ref rn3 1
    linkNodes r2 $ Ref rn3 2
    reassocCont r0 $ Ref rn3 0
reduceNode n0@PArgumentNode {} n1@Bind0BNode {} = reduceNode n1 n0
reduceNode (Bind0CNode _ r0 r1) (IOPureNode _ r2) = do
    rn3 <- newNode $ AppNode () () ()
    linkNodes r0 $ Ref rn3 1
    linkNodes r1 $ Ref rn3 2
    linkNodes r2 $ Ref rn3 0
reduceNode n0@IOPureNode {} n1@Bind0CNode {} = reduceNode n1 n0
reduceNode (Bind0FNode name _ r0 r1) (IOContNode instr _) = do
    let op = B.Reference (B.instrType instr) name
    rn2 <- newNode $ Bind1FNode (name B.:= instr) () ()
    rn3 <- newNode $ AppNode () () ()
    rn4 <- newNode $ OperandNode op ()
    linkNodes (Ref rn2 1) (Ref rn3 2)
    linkNodes (Ref rn3 1) (Ref rn4 0)
    linkNodes r0 $ Ref rn3 0
    linkNodes r1 $ Ref rn2 0
reduceNode n0@IOContNode {} n1@Bind0FNode {} = reduceNode n1 n0
reduceNode (Branch0FNode _ r0 r1 r2) (OperandNode op _) = mkBranch1 op r0 r1 r2
reduceNode n0@OperandNode {} n1@Branch0FNode {} = reduceNode n1 n0
reduceNode (LabelNode lbl _ r0) (IONode bs _) = propagate1 r0 $ NamedBlockNode
    $ B.NamedBlockList $ IM.insert (B.unLabel lbl) (B.entryBlock bs)
    $ B.unNamedBlockList $ B.namedBlocks bs
reduceNode n0@IONode {} n1@LabelNode {} = reduceNode n1 n0
reduceNode (Merge0Node _ r0 r1) (NamedBlockNode nbs _) = do
    rn2 <- newNode $ Merge1Node nbs () ()
    linkNodes r0 $ Ref rn2 0
    linkNodes r1 $ Ref rn2 1
reduceNode n0@NamedBlockNode {} n1@Merge0Node {} = reduceNode n1 n0
reduceNode (Merge1Node nbs0 _ r0) (NamedBlockNode nbs1 _)
    = propagate1 r0 $ NamedBlockNode $ nbs0 <> nbs1
reduceNode n0@NamedBlockNode {} n1@Merge1Node {} = reduceNode n1 n0
reduceNode (TBuildNode _ _ _ _ r0 r1 r2) (OperandNode op _)
    = propagate2 r0 r1 TCloseNode *> propagate1 r2 (OperandNode op)
reduceNode n0@OperandNode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (TBuildNode _ _ _ _ r0 r1 r2) (OperandANode opp _)
    = propagate2 r0 r1 TCloseNode *> propagate1 r2 (OperandANode opp)
reduceNode n0@OperandANode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (TBuildNode _ _ _ _ r0 r1 r2) (IONode bs _) = do
    propagate2 r0 r1 TCloseNode
    propagate1 r2 $ IONode bs
reduceNode n0@IONode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (TBuildNode _ _ _ _ r0 r1 r2) (IOANode iop _)
    = propagate2 r0 r1 TCloseNode *> propagate1 r2 (IOANode iop)
reduceNode n0@IOANode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (TBuildNode _ _ _ _ r0 r1 r2) (IOContNode instr _)
    = propagate2 r0 r1 TCloseNode *> propagate1 r2 (IOContNode instr)
reduceNode n0@IOContNode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (TBuildNode lvl _ namep _ r0 r1 r2) (ReturnCNode _ r3) = do
    rn4 <- newNode $ TBuildNode lvl BuildOperand namep () () () ()
    rn5 <- newNode $ ReturnFNode () ()
    rn6 <- newNode $ PReduceNode () ()
    linkNodes (Ref rn4 3) (Ref rn6 0)
    linkNodes (Ref rn5 0) (Ref rn6 1)
    linkNodes r0 $ Ref rn4 1
    linkNodes r1 $ Ref rn4 2
    linkNodes r2 $ Ref rn5 1
    linkNodes r3 $ Ref rn4 0
reduceNode n0@ReturnCNode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (TBuildNode lvl _ namep _ r0 r1 r2) (Bind1CNode _ r3 r4) = do
    name <- newName
    rn5 <- newNode $ TBuildNode lvl BuildOperand namep () () () ()
    rn6 <- newNode $ TBuildNode lvl BuildIO namep () () () ()
    rn7 <- newNode $ Bind0FNode name () () ()
    rn8 <- newNode $ TSplitNode () () ()
    rn9 <- newNode $ TSplitNode () () ()
    rn10 <- newNode $ PReduceNode () ()
    linkNodes (Ref rn5 1) (Ref rn8 1)
    linkNodes (Ref rn5 2) (Ref rn9 1)
    linkNodes (Ref rn5 3) (Ref rn10 0)
    linkNodes (Ref rn6 1) (Ref rn8 2)
    linkNodes (Ref rn6 2) (Ref rn9 2)
    linkNodes (Ref rn6 0) (Ref rn7 2)
    linkNodes (Ref rn7 0) (Ref rn10 1)
    linkNodes r0 $ Ref rn8 0
    linkNodes r1 $ Ref rn9 0
    linkNodes r2 $ Ref rn6 3
    linkNodes r3 $ Ref rn5 0
    linkNodes r4 $ Ref rn7 1
reduceNode n0@Bind1CNode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (TBuildNode lvl _ namep _ r0 r1 r2) (Bind1FNode instr _ r3) = do
    rn4 <- newNode $ TBuildNode lvl BuildIO namep () () () ()
    rn5 <- newNode $ Bind1FNode instr () ()
    linkNodes (Ref rn4 3) (Ref rn5 1)
    linkNodes r0 $ Ref rn4 1
    linkNodes r1 $ Ref rn4 2
    linkNodes r2 $ Ref rn5 0
    linkNodes r3 $ Ref rn4 0
reduceNode n0@Bind1FNode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (TBuildNode lvl _ namep _ r0 r1 r2) (Branch0CNode _ r3 r4 r5) = do
    rn6 <- newNode $ TBuildNode lvl BuildOperand namep () () () ()
    rn7 <- newNode $ TBuildNode lvl BuildIO namep () () () ()
    rn8 <- newNode $ TBuildNode lvl BuildIO namep () () () ()
    rn9 <- newNode $ Branch0CNode () () () ()
    rn10 <- newNode $ TSplitNode () () ()
    rn11 <- newNode $ TSplitNode () () ()
    rn12 <- newNode $ TSplitNode () () ()
    rn13 <- newNode $ TSplitNode () () ()
    linkNodes (Ref rn6 1) (Ref rn10 1)
    linkNodes (Ref rn6 2) (Ref rn11 1)
    linkNodes (Ref rn6 3) (Ref rn9 1)
    linkNodes (Ref rn7 1) (Ref rn12 1)
    linkNodes (Ref rn7 2) (Ref rn13 1)
    linkNodes (Ref rn7 3) (Ref rn9 2)
    linkNodes (Ref rn8 1) (Ref rn12 2)
    linkNodes (Ref rn8 2) (Ref rn13 2)
    linkNodes (Ref rn8 3) (Ref rn9 3)
    linkNodes (Ref rn10 2) (Ref rn12 0)
    linkNodes (Ref rn11 2) (Ref rn13 0)
    linkNodes r0 $ Ref rn10 0
    linkNodes r1 $ Ref rn11 0
    linkNodes r2 $ Ref rn9 0
    linkNodes r3 $ Ref rn6 0
    linkNodes r4 $ Ref rn7 0
    linkNodes r5 $ Ref rn8 0
reduceNode n0@Branch0CNode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (TBuildNode lvl _ namep _ r0 r1 r2) (PArgumentNode _ r3 r4) = do
    rn5 <- newNode $ TBuildNode lvl BuildOperand namep () () () ()
    rn6 <- newNode $ TBuildNode lvl BuildOperand namep () () () ()
    rn7 <- newNode $ PArgumentNode () () ()
    rn8 <- newNode $ TSplitNode () () ()
    rn9 <- newNode $ TSplitNode () () ()
    linkNodes (Ref rn5 1) (Ref rn8 1)
    linkNodes (Ref rn5 2) (Ref rn9 1)
    linkNodes (Ref rn5 3) (Ref rn7 1)
    linkNodes (Ref rn6 1) (Ref rn8 2)
    linkNodes (Ref rn6 2) (Ref rn9 2)
    linkNodes (Ref rn6 3) (Ref rn7 2)
    linkNodes r0 $ Ref rn8 0
    linkNodes r1 $ Ref rn9 0
    linkNodes r2 $ Ref rn7 0
    linkNodes r3 $ Ref rn5 0
    linkNodes r4 $ Ref rn6 0
reduceNode n0@PArgumentNode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (AccumIONode ib _ r0) (TEntryNode name opp _ r1) = do
    let term = B.TailCall name opp
    propagate1 r0 $ IONode $ B.BlockList (B.Block (B.unIBlock ib) term) mempty
    propagate1 r1 TCloseNode
reduceNode n0@TEntryNode {} n1@AccumIONode {} = reduceNode n1 n0
reduceNode (TBuildNode lvl t namep _ r0 r1 r2) (TEntryNode name opp _ r3) = do
    rn4 <- newNode $ TBuildNode lvl t namep () () () ()
    rn5 <- newNode $ TEntryNode name opp () ()
    linkNodes (Ref rn4 3) (Ref rn5 1)
    linkNodes r0 $ Ref rn4 1
    linkNodes r1 $ Ref rn4 2
    linkNodes r2 $ Ref rn5 0
    linkNodes r3 $ Ref rn4 0
reduceNode n0@TEntryNode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (TBuildNode lvl t namep _ r0 r1 r2) (TSplitNode _ r3 r4)
    = commute2a (TBuildNode lvl t namep) TSplitNode r0 r1 r2 r3 r4
reduceNode n0@TSplitNode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (TBuildNode _ _ _ _ r0 r1 r2) (TCloseNode _)
    = propagate3 r0 r1 r2 TCloseNode
reduceNode n0@TCloseNode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (TSplitNode _ r0 r1) (TCloseNode _) = propagate2 r0 r1 TCloseNode
reduceNode n0@TCloseNode {} n1@TSplitNode {} = reduceNode n1 n0
reduceNode (TCloseNode _) (TCloseNode _) = pure ()
reduceNode (TBuildNode lvl _ namep _ r0 r1 r2) (TLeaveNode t _ r3 r4) = do
    rn5 <- newNode $ TBuildNode lvl t namep () () () ()
    rn6 <- newNode $ TLeaveNode t () () ()
    linkNodes (Ref rn5 3) (Ref rn6 1)
    linkNodes r0 $ Ref rn5 1
    linkNodes r1 $ Ref rn5 2
    linkNodes r2 $ Ref rn6 0
    linkNodes r3 $ Ref rn5 0
    linkNodes r4 $ Ref rn6 2
reduceNode n0@TLeaveNode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (TLeaveNode _ _ r0 r1) (TCloseNode _) = linkNodes r0 r1
reduceNode n0@TCloseNode {} n1@TLeaveNode {} = reduceNode n1 n0
reduceNode (TBuildNode lvl0 t namep _ r0 r1 r2) (TMatchNode lvl1 _ r3 r4 r5)
    | lvl0 == lvl1 = do
        linkNodes r0 r3
        linkNodes r1 r4
        propagate1 r2 TCloseNode
        propagate1 r5 $ OperandNode $ B.Reference (B.IntType 1) namep
    | otherwise = do
        let opp = B.Partial (SSucc $ SSucc SZero) $ mkSelect
                $ B.Reference (B.IntType 1) namep
        rn6 <- newNode $ TBuildNode lvl0 t namep () () () ()
        rn7 <- newNode $ TBuildNode lvl0 t namep () () () ()
        rn8 <- newNode $ TMatchNode lvl1 () () () ()
        rn9 <- newNode $ TMatchNode lvl1 () () () ()
        rn10 <- newNode $ OperandPNode opp () ()
        rn11 <- newNode $ AppNode () () ()
        linkNodes (Ref rn6 1) (Ref rn8 1)
        linkNodes (Ref rn6 2) (Ref rn9 1)
        propagate1 (Ref rn6 3) TCloseNode
        linkNodes (Ref rn7 1) (Ref rn8 2)
        linkNodes (Ref rn7 2) (Ref rn9 2)
        propagate1 (Ref rn7 3) TCloseNode
        linkNodes (Ref rn8 3) (Ref rn10 0)
        linkNodes (Ref rn9 3) (Ref rn11 1)
        linkNodes (Ref rn10 1) (Ref rn11 0)
        linkNodes r0 $ Ref rn8 0
        linkNodes r1 $ Ref rn9 0
        propagate1 r2 TCloseNode
        linkNodes r3 $ Ref rn6 0
        linkNodes r4 $ Ref rn7 0
        linkNodes r5 $ Ref rn11 2
reduceNode n0@TMatchNode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (PArgumentNode _ r0 r1) (AppNode _ r2 r3) = do
    rn4 <- newNode $ PArgumentNode () () ()
    rn5 <- newNode $ PArgumentNode () () ()
    linkNodes (Ref rn4 0) (Ref rn5 1)
    linkNodes r0 $ Ref rn4 1
    linkNodes r1 $ Ref rn4 2
    linkNodes r2 $ Ref rn5 2
    linkNodes r3 $ Ref rn5 0
reduceNode n0@AppNode {} n1@PArgumentNode {} = reduceNode n1 n0
reduceNode (PArgumentNode _ r0 r1) (OperandPNode opp _ r2) = do
    rn3 <- newNode $ AppNode () () ()
    rn4 <- newNode $ PReduceNode () ()
    rn5 <- newNode $ PReduceNode () ()
    rn6 <- newNode $ OperandPNode opp () ()
    linkNodes (Ref rn3 0) (Ref rn4 1)
    linkNodes (Ref rn3 1) (Ref rn5 1)
    linkNodes (Ref rn3 2) (Ref rn6 0)
    linkNodes r0 $ Ref rn4 0
    linkNodes r1 $ Ref rn5 0
    linkNodes r2 $ Ref rn6 1
reduceNode n0@OperandPNode {} n1@PArgumentNode {} = reduceNode n1 n0
reduceNode (PArgumentNode _ r0 r1) (IOPNode iop _ r2) = do
    rn3 <- newNode $ AppNode () () ()
    rn4 <- newNode $ PReduceNode () ()
    rn5 <- newNode $ PReduceNode () ()
    rn6 <- newNode $ IOPNode iop () ()
    linkNodes (Ref rn3 0) (Ref rn4 1)
    linkNodes (Ref rn3 1) (Ref rn5 1)
    linkNodes (Ref rn3 2) (Ref rn6 0)
    linkNodes r0 $ Ref rn4 0
    linkNodes r1 $ Ref rn5 0
    linkNodes r2 $ Ref rn6 1
reduceNode n0@IOPNode {} n1@PArgumentNode {} = reduceNode n1 n0
reduceNode (PArgumentNode _ r0 r1) (PReduceNode _ r2) = do
    rn3 <- newNode $ AppNode () () ()
    rn4 <- newNode $ PReduceNode () ()
    rn5 <- newNode $ PReduceNode () ()
    linkNodes (Ref rn3 0) (Ref rn4 1)
    linkNodes (Ref rn3 1) (Ref rn5 1)
    linkNodes r0 $ Ref rn4 0
    linkNodes r1 $ Ref rn5 0
    linkNodes r2 $ Ref rn3 2
reduceNode n0@PReduceNode {} n1@PArgumentNode {} = reduceNode n1 n0
reduceNode (PReduceNode _ r0) (OperandNode op _)
    = propagate1 r0 $ OperandNode op
reduceNode n0@OperandNode {} n1@PReduceNode {} = reduceNode n1 n0
reduceNode (PReduceNode _ r0) (OperandANode opp _)
    = mkLambda r0 $ OperandPNode opp
reduceNode n0@OperandANode {} n1@PReduceNode {} = reduceNode n1 n0
reduceNode (PReduceNode _ r0) (IOANode iop _) = mkLambda r0 $ IOPNode iop
reduceNode n0@IOANode {} n1@PReduceNode {} = reduceNode n1 n0
reduceNode (PReduceNode _ r0) (IOContNode instr _)
    = propagate1 r0 $ IOContNode instr
reduceNode n0@IOContNode {} n1@PReduceNode {} = reduceNode n1 n0
-- FFI Duplication
reduceNode (DupNode _ _ r0 r1) (OperandNode op _)
    = propagate2 r0 r1 $ OperandNode op
reduceNode n0@OperandNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode _ _ r0 r1) (OperandANode opp _)
    = propagate2 r0 r1 $ OperandANode opp
reduceNode n0@OperandANode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl _ r0 r1) (OperandPNode opp _ r2)
    = commute1 (DupNode lvl) (OperandPNode opp) r0 r1 r2
reduceNode n0@OperandPNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl _ r0 r1) (IONode bs _) = do
    rn2 <- newNode $ IONode bs ()
    dedupIO lvl r0 r1 $ Ref rn2 0
reduceNode n0@IONode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode _ _ r0 r1) (IOANode iop _) = propagate2 r0 r1 $ IOANode iop
reduceNode n0@IOANode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl _ r0 r1) (IOPNode iop _ r2)
    = commute1 (DupNode lvl) (IOPNode iop) r0 r1 r2
reduceNode n0@IOPNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl _ r0 r1) (IOPureNode _ r2)
    = commute1 (DupNode lvl) IOPureNode r0 r1 r2
reduceNode n0@IOPureNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode _ _ r0 r1) (IOContNode instr _)
    = propagate2 r0 r1 $ IOContNode instr
reduceNode n0@IOContNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl _ r0 r1) (ReturnCNode _ r2) = do
    rn3 <- newNode $ ReturnCNode () ()
    linkNodes r2 $ Ref rn3 1
    dedupIO lvl r0 r1 $ Ref rn3 0
reduceNode n0@ReturnCNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl _ r0 r1) (Bind0BNode _ r2)
    = commute1 (DupNode lvl) Bind0BNode r0 r1 r2
reduceNode n0@Bind0BNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl _ r0 r1) (Bind0CNode _ r2 r3)
    = commute2 (DupNode lvl) Bind0CNode r0 r1 r2 r3
reduceNode n0@Bind0CNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl _ r0 r1) (Bind1CNode _ r2 r3) = do
    rn4 <- newNode $ Bind1CNode () () ()
    linkNodes r2 $ Ref rn4 1
    linkNodes r3 $ Ref rn4 2
    dedupIO lvl r0 r1 $ Ref rn4 0
reduceNode n0@Bind1CNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl _ r0 r1) (Branch0CNode _ r2 r3 r4) = do
    rn5 <- newNode $ Branch0CNode () () () ()
    linkNodes r2 $ Ref rn5 1
    linkNodes r3 $ Ref rn5 2
    linkNodes r4 $ Ref rn5 3
    dedupIO lvl r0 r1 $ Ref rn5 0
reduceNode n0@Branch0CNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl0 _ r0 r1) (TBuildNode lvl1 t namep _ r2 r3 r4)
    | lvl0 == lvl1 = do
        let opp = B.Reference (B.IntType 1) namep
        rn5 <- newNode $ TLeaveNode t () () ()
        rn6 <- newNode $ TLeaveNode t () () ()
        linkNodes r0 $ Ref rn5 1
        linkNodes r1 $ Ref rn6 1
        linkNodes r2 $ Ref rn5 0
        linkNodes r3 $ Ref rn6 0
        case t of
            BuildOperand -> do
                let opp' = B.Partial (SSucc $ SSucc SZero) $ mkSelect opp
                rn7 <- newNode $ OperandPNode opp' () ()
                rn8 <- newNode $ AppNode () () ()
                linkNodes (Ref rn5 2) (Ref rn7 0)
                linkNodes (Ref rn6 2) (Ref rn8 1)
                linkNodes (Ref rn7 1) (Ref rn8 0)
                linkNodes r4 $ Ref rn8 2
            BuildIO -> mkBranch1 opp (Ref rn5 2) (Ref rn6 2) r4
    | otherwise = do
        let opp = B.Partial (SSucc $ SSucc SZero)
                $ mkSelect $ B.Reference (B.IntType 1) namep
        rn3 <- newNode $ OperandPNode opp () ()
        rn4 <- newNode $ AppNode () () ()
        rn5 <- newNode $ TMatchNode lvl0 () () () ()
        rn6 <- newNode $ TMatchNode lvl0 () () () ()
        rn7 <- newNode $ TBuildNode lvl1 t namep () () () ()
        rn8 <- newNode $ TBuildNode lvl1 t namep () () () ()
        linkNodes (Ref rn3 0) (Ref rn5 3)
        linkNodes (Ref rn3 1) (Ref rn4 0)
        linkNodes (Ref rn4 1) (Ref rn6 3)
        linkNodes (Ref rn5 1) (Ref rn7 1)
        linkNodes (Ref rn5 2) (Ref rn8 1)
        linkNodes (Ref rn6 1) (Ref rn7 2)
        linkNodes (Ref rn6 2) (Ref rn8 2)
        linkNodes r0 $ Ref rn7 0
        linkNodes r1 $ Ref rn8 0
        linkNodes r2 $ Ref rn5 0
        linkNodes r3 $ Ref rn6 0
        case t of
            BuildOperand -> do
                let opp' = B.Partial (SSucc $ SSucc $ SSucc SZero) mkSelect
                rn2 <- newNode $ OperandPNode opp' () ()
                rn9 <- newNode $ AppNode () () ()
                rn10 <- newNode $ AppNode () () ()
                linkNodes (Ref rn2 0) (Ref rn4 2)
                linkNodes (Ref rn2 1) (Ref rn9 0)
                linkNodes (Ref rn7 3) (Ref rn9 1)
                linkNodes (Ref rn8 3) (Ref rn10 1)
                linkNodes (Ref rn9 2) (Ref rn10 0)
                linkNodes r4 $ Ref rn10 2
            BuildIO -> do
                rn2 <- newNode $ Branch0CNode () () () ()
                linkNodes (Ref rn2 1) (Ref rn4 2)
                linkNodes (Ref rn2 2) (Ref rn7 3)
                linkNodes (Ref rn2 3) (Ref rn8 3)
                linkNodes r4 $ Ref rn2 0
reduceNode n0@TBuildNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl _ r0 r1) (TEntryNode name opp _ r2) = do
    rn3 <- newNode $ TEntryNode name opp () ()
    linkNodes r2 $ Ref rn3 1
    dedupIO lvl r0 r1 $ Ref rn3 0
reduceNode n0@TEntryNode {} n1@DupNode {} = reduceNode n1 n0
reduceNode (DupNode lvl _ r0 r1) (PArgumentNode _ r2 r3)
    = commute2 (DupNode lvl) PArgumentNode r0 r1 r2 r3
reduceNode n0@PArgumentNode {} n1@DupNode {} = reduceNode n1 n0
-- FFI Dead
reduceNode (AccumIONode ib _ r0) (DeadNode _) = propagate1 r0
    $ IONode $ B.BlockList (B.Block (B.unIBlock ib) B.Unreachable) mempty
reduceNode n0@DeadNode {} n1@AccumIONode {} = reduceNode n1 n0
reduceNode (OperandNode _ _) (DeadNode _) = pure ()
reduceNode n0@DeadNode {} n1@OperandNode {} = reduceNode n1 n0
reduceNode (OperandPNode _ _ r0) (DeadNode _) = propagate1 r0 DeadNode
reduceNode n0@DeadNode {} n1@OperandPNode {} = reduceNode n1 n0
reduceNode (IONode _ _) (DeadNode _) = pure ()
reduceNode n0@DeadNode {} n1@IONode {} = reduceNode n1 n0
reduceNode (IOPNode _ _ r0) (DeadNode _) = propagate1 r0 DeadNode
reduceNode n0@DeadNode {} n1@IOPNode {} = reduceNode n1 n0
reduceNode (IOPureNode _ r0) (DeadNode _) = propagate1 r0 DeadNode
reduceNode n0@DeadNode {} n1@IOPureNode {} = reduceNode n1 n0
reduceNode (IOContNode _ _) (DeadNode _) = pure ()
reduceNode n0@DeadNode {} n1@IOContNode {} = reduceNode n1 n0
reduceNode (Bind0BNode _ r0) (DeadNode _) = propagate1 r0 DeadNode
reduceNode n0@DeadNode {} n1@Bind0BNode {} = reduceNode n1 n0
reduceNode (Bind0CNode _ r0 r1) (DeadNode _) = propagate2 r0 r1 DeadNode
reduceNode n0@DeadNode {} n1@Bind0CNode {} = reduceNode n1 n0
reduceNode (Bind1FNode _ _ r0) (DeadNode _) = propagate1 r0 DeadNode
reduceNode n0@DeadNode {} n1@Bind1FNode {} = reduceNode n1 n0
reduceNode (LabelNode lbl _ r0) (DeadNode _)
    = propagate1 r0 $ NamedBlockNode $ B.NamedBlockList
        $ IM.singleton (B.unLabel lbl) $ B.Block mempty B.Unreachable
reduceNode n0@DeadNode {} n1@LabelNode {} = reduceNode n1 n0
reduceNode (NamedBlockNode _ _) (DeadNode _) = pure ()
reduceNode n0@DeadNode {} n1@NamedBlockNode {} = reduceNode n1 n0
reduceNode (TBuildNode _ _ _ _ r0 r1 r2) (DeadNode _)
    = propagate2 r0 r1 TCloseNode *> propagate1 r2 DeadNode
reduceNode n0@DeadNode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (TEntryNode _ _ _ r0) (DeadNode _) = propagate1 r0 TCloseNode
reduceNode n0@DeadNode {} n1@TEntryNode {} = reduceNode n1 n0
-- FFI Book-keeping
reduceNode (AccumIONode ib _ r0) (BoxNode _ _ r1) = do
    rn2 <- newNode $ AccumIONode ib () ()
    linkNodes r0 $ Ref rn2 1
    linkNodes r1 $ Ref rn2 0
reduceNode n0@BoxNode {} n1@AccumIONode {} = reduceNode n1 n0
reduceNode (OperandNode op _) (BoxNode _ _ r0) = propagate1 r0 $ OperandNode op
reduceNode n0@BoxNode {} n1@OperandNode {} = reduceNode n1 n0
reduceNode (OperandANode opp _) (BoxNode _ _ r0)
    = propagate1 r0 $ OperandANode opp
reduceNode n0@BoxNode {} n1@OperandANode {} = reduceNode n1 n0
reduceNode (OperandPNode opp _ r0) (BoxNode lvl _ r1)
    = commute0 (OperandPNode opp) (BoxNode lvl) r0 r1
reduceNode n0@BoxNode {} n1@OperandPNode {} = reduceNode n1 n0
reduceNode (IONode b _) (BoxNode _ _ r0) = propagate1 r0 $ IONode b
reduceNode n0@BoxNode {} n1@IONode {} = reduceNode n1 n0
reduceNode (IOANode iop _) (BoxNode _ _ r0) = propagate1 r0 $ IOANode iop
reduceNode n0@BoxNode {} n1@IOANode {} = reduceNode n1 n0
reduceNode (IOPNode iop _ r0) (BoxNode lvl _ r1)
    = commute0 (IOPNode iop) (BoxNode lvl) r0 r1
reduceNode n0@BoxNode {} n1@IOPNode {} = reduceNode n1 n0
reduceNode (IOPureNode _ r0) (BoxNode lvl _ r1)
    = commute0 IOPureNode (BoxNode lvl) r0 r1
reduceNode n0@BoxNode {} n1@IOPureNode {} = reduceNode n1 n0
reduceNode (IOContNode instr _) (BoxNode _ _ r0)
    = propagate1 r0 $ IOContNode instr
reduceNode n0@BoxNode {} n1@IOContNode {} = reduceNode n1 n0
reduceNode (ReturnCNode _ r0) (BoxNode lvl _ r1)
    = commute0 ReturnCNode (BoxNode lvl) r0 r1
reduceNode n0@BoxNode {} n1@ReturnCNode {} = reduceNode n1 n0
reduceNode (ReturnFNode _ r0) (BoxNode lvl _ r1)
    = commute0 ReturnFNode (BoxNode lvl) r0 r1
reduceNode n0@BoxNode {} n1@ReturnFNode {} = reduceNode n1 n0
reduceNode (Bind0BNode _ r0) (BoxNode lvl _ r1)
    = commute0 Bind0BNode (BoxNode lvl) r0 r1
reduceNode n0@BoxNode {} n1@Bind0BNode {} = reduceNode n1 n0
reduceNode (Bind0CNode _ r0 r1) (BoxNode lvl _ r2)
    = commute1 Bind0CNode (BoxNode lvl) r0 r1 r2
reduceNode n0@BoxNode {} n1@Bind0CNode {} = reduceNode n1 n0
reduceNode (Bind0FNode name _ r0 r1) (BoxNode lvl _ r2)
    = commute1 (Bind0FNode name) (BoxNode lvl) r0 r1 r2
reduceNode n0@BoxNode {} n1@Bind0FNode {} = reduceNode n1 n0
reduceNode (Bind1CNode _ r0 r1) (BoxNode lvl _ r2)
    = commute1 Bind1CNode (BoxNode lvl) r0 r1 r2
reduceNode n0@BoxNode {} n1@Bind1CNode {} = reduceNode n1 n0
reduceNode (Bind1FNode nbs _ r0) (BoxNode lvl _ r1)
    = commute0 (Bind1FNode nbs) (BoxNode lvl) r0 r1
reduceNode n0@BoxNode {} n1@Bind1FNode {} = reduceNode n1 n0
reduceNode (Branch0CNode _ r0 r1 r2) (BoxNode lvl _ r3)
    = commute2b Branch0CNode (BoxNode lvl) r0 r1 r2 r3
reduceNode n0@BoxNode {} n1@Branch0CNode {} = reduceNode n1 n0
reduceNode (Branch0FNode _ r0 r1 r2) (BoxNode lvl _ r3)
    = commute2b Branch0FNode (BoxNode lvl) r0 r1 r2 r3
reduceNode n0@BoxNode {} n1@Branch0FNode {} = reduceNode n1 n0
reduceNode (LabelNode lbl _ r0) (BoxNode _ _ r1)
    = do
    rn2 <- newNode $ LabelNode lbl () ()
    linkNodes r0 $ Ref rn2 1
    linkNodes r1 $ Ref rn2 0
reduceNode n0@BoxNode {} n1@LabelNode {} = reduceNode n1 n0
reduceNode (TBuildNode lvl0 t namep _ r0 r1 r2) (BoxNode lvl1 _ r3) = do
    let lvl0' = if lvl0 < lvl1 then lvl0 else succ lvl0
    rn4 <- newNode $ TBuildNode lvl0' t namep () () () ()
    rn5 <- newNode $ BoxNode lvl1 () ()
    rn6 <- newNode $ BoxNode lvl1 () ()
    linkNodes (Ref rn4 1) (Ref rn5 1)
    linkNodes (Ref rn4 2) (Ref rn6 1)
    linkNodes r0 $ Ref rn5 0
    linkNodes r1 $ Ref rn6 0
    linkNodes r2 $ Ref rn4 3
    linkNodes r3 $ Ref rn4 0
reduceNode n0@BoxNode {} n1@TBuildNode {} = reduceNode n1 n0
reduceNode (TEntryNode name opp _ r0) (BoxNode lvl _ r1)
    = commute0 (TEntryNode name opp) (BoxNode lvl) r0 r1
reduceNode n0@BoxNode {} n1@TEntryNode {} = reduceNode n1 n0
reduceNode (TSplitNode _ r0 r1) (BoxNode lvl _ r2)
    = commute1 TSplitNode (BoxNode lvl) r0 r1 r2
reduceNode n0@BoxNode {} n1@TSplitNode {} = reduceNode n1 n0
reduceNode (TCloseNode _) (BoxNode _ _ r0) = propagate1 r0 TCloseNode
reduceNode n0@BoxNode {} n1@TCloseNode {} = reduceNode n1 n0
reduceNode (TLeaveNode t _ r0 r1) (BoxNode lvl _ r2) = do
    rn3 <- newNode $ TLeaveNode t () () ()
    rn4 <- newNode $ BoxNode lvl () ()
    linkNodes (Ref rn3 1) (Ref rn4 1)
    linkNodes r0 $ Ref rn4 0
    linkNodes r1 $ Ref rn3 2
    linkNodes r2 $ Ref rn3 0
reduceNode n0@BoxNode {} n1@TLeaveNode {} = reduceNode n1 n0
reduceNode (TMatchNode lvl0 _ r0 r1 r2) (BoxNode lvl1 _ r3) = commute2b
    (TMatchNode $ if lvl0 < lvl1 then lvl0 else succ lvl0)
    (BoxNode lvl1)
    r0 r1 r2 r3
reduceNode n0@BoxNode {} n1@TMatchNode {} = reduceNode n1 n0
reduceNode (PArgumentNode _ r0 r1) (BoxNode lvl _ r2)
    = commute1 PArgumentNode (BoxNode lvl) r0 r1 r2
reduceNode n0@BoxNode {} n1@PArgumentNode {} = reduceNode n1 n0
reduceNode (PReduceNode _ r0) (BoxNode _ _ r1) = do
    rn2 <- newNode $ PReduceNode () ()
    linkNodes r0 $ Ref rn2 1
    linkNodes r1 $ Ref rn2 0
reduceNode n0@BoxNode {} n1@PReduceNode {} = reduceNode n1 n0
reduceNode n0 n1 = error . show
    $ "unexpected node pairing" <> line <> pretty n0 <> line <> pretty n1
{-# INLINABLE reduceNode #-}

commute0
    :: HasRewriter sig m
    => (() -> () -> INetF ()) -> (() -> () -> INetF ())
    -> Ref -> Ref -> m ()
commute0 mk1 mk2 r0 r1 = do
    rn3 <- newNode $ mk2 () ()
    rn4 <- newNode $ mk1 () ()
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
    rn3 <- newNode $ mk2a () ()
    rn4 <- newNode $ mk2b () ()
    rn5 <- newNode $ mk1 () () ()
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
    rn4 <- newNode $ mk1a () () ()
    rn5 <- newNode $ mk1b () () ()
    rn6 <- newNode $ mk2a () () ()
    rn7 <- newNode $ mk2b () () ()
    linkNodes (Ref rn4 1) (Ref rn6 1)
    linkNodes (Ref rn4 2) (Ref rn7 1)
    linkNodes (Ref rn5 1) (Ref rn6 2)
    linkNodes (Ref rn5 2) (Ref rn7 2)
    linkNodes r0 $ Ref rn6 0
    linkNodes r1 $ Ref rn7 0
    linkNodes r2 $ Ref rn4 0
    linkNodes r3 $ Ref rn5 0
{-# INLINABLE commute2' #-}

commute2a
    :: HasRewriter sig m
    => (() -> () -> () -> () -> INetF ())
    -> (() -> () -> () -> INetF ())
    -> Ref -> Ref -> Ref -> Ref -> Ref -> m ()
commute2a mk1 mk2 = commute2a' mk1 mk2 mk2 mk2
{-# INLINABLE commute2a #-}

commute2a'
    :: HasRewriter sig m
    => (() -> () -> () -> () -> INetF ())
    -> (() -> () -> () -> INetF ())
    -> (() -> () -> () -> INetF ())
    -> (() -> () -> () -> INetF ())
    -> Ref -> Ref -> Ref -> Ref -> Ref -> m ()
commute2a' mk1 mk2a mk2b mk2c r0 r1 r2 r3 r4 = do
    rn5 <- newNode $ mk1 () () () ()
    rn6 <- newNode $ mk1 () () () ()
    rn7 <- newNode $ mk2a () () ()
    rn8 <- newNode $ mk2b () () ()
    rn9 <- newNode $ mk2c () () ()
    linkNodes (Ref rn5 1) (Ref rn7 1)
    linkNodes (Ref rn5 2) (Ref rn8 1)
    linkNodes (Ref rn5 3) (Ref rn9 1)
    linkNodes (Ref rn6 1) (Ref rn7 2)
    linkNodes (Ref rn6 2) (Ref rn8 2)
    linkNodes (Ref rn6 3) (Ref rn9 2)
    linkNodes r0 $ Ref rn7 0
    linkNodes r1 $ Ref rn8 0
    linkNodes r2 $ Ref rn9 0
    linkNodes r3 $ Ref rn5 0
    linkNodes r4 $ Ref rn6 0
{-# INLINABLE commute2a' #-}

commute2b
    :: HasRewriter sig m
    => (() -> () -> () -> () -> INetF ())
    -> (() -> () -> INetF ())
    -> Ref -> Ref -> Ref -> Ref -> m ()
commute2b mk1 mk2 r0 r1 r2 r3 = do
    rn4 <- newNode $ mk1 () () () ()
    rn5 <- newNode $ mk2 () ()
    rn6 <- newNode $ mk2 () ()
    rn7 <- newNode $ mk2 () ()
    linkNodes (Ref rn4 1) (Ref rn5 1)
    linkNodes (Ref rn4 2) (Ref rn6 1)
    linkNodes (Ref rn4 3) (Ref rn7 1)
    linkNodes r0 $ Ref rn5 0
    linkNodes r1 $ Ref rn6 0
    linkNodes r2 $ Ref rn7 0
    linkNodes r3 $ Ref rn4 0
{-# INLINABLE commute2b #-}

propagate1 :: HasRewriter sig m => Ref -> (() -> INetF ()) -> m ()
propagate1 r0 mk1 = do
    rn1 <- newNode $ mk1 ()
    linkNodes r0 $ Ref rn1 0
{-# INLINABLE propagate1 #-}

propagate2 :: HasRewriter sig m => Ref -> Ref -> (() -> INetF ()) -> m ()
propagate2 r0 r1 mk1 = propagate1 r0 mk1 *> propagate1 r1 mk1
{-# INLINABLE propagate2 #-}

propagate3 :: HasRewriter sig m => Ref -> Ref -> Ref -> (() -> INetF ()) -> m ()
propagate3 r0 r1 r2 mk1 = propagate2 r0 r1 mk1 *> propagate1 r2 mk1
{-# INLINABLE propagate3 #-}

dedupIO :: HasRewriter sig m => Level -> Ref -> Ref -> Ref -> m ()
dedupIO lvl r0 r1 r2 = do
    name <- newName
    namep <- newName
    rn3 <- newNode $ TEntryNode name (B.Constant B.B1) () ()
    rn4 <- newNode $ TEntryNode name (B.Constant B.B0) () ()
    rn5 <- newNode $ TBuildNode lvl BuildIO namep () () () ()
    rn6 <- newNode $ AccumIONode mempty () ()
    linkNodes (Ref rn3 1) (Ref rn5 1)
    linkNodes (Ref rn4 1) (Ref rn5 2)
    linkNodes (Ref rn5 3) (Ref rn6 0)
    propagate1 (Ref rn6 1) $ PrivateRootNode name namep
    linkNodes r0 $ Ref rn3 0
    linkNodes r1 $ Ref rn4 0
    linkNodes r2 $ Ref rn5 0
{-# INLINABLE dedupIO #-}

reassocPure :: HasRewriter sig m => Ref -> Ref -> m ()
reassocPure r0 r1 = do
    rn2 <- newNode $ LamNode () () ()
    rn3 <- newNode $ IOPureNode () ()
    rn4 <- newNode $ LamNode () () ()
    rn5 <- newNode $ AppNode () () ()
    rn6 <- newNode $ LamNode () () ()
    rn7 <- newNode $ Bind0CNode () () ()
    rn8 <- newNode $ AppNode () () ()
    rn9 <- newNode $ BoxNode 0 () ()
    rn10 <- newNode $ BoxNode 0 () ()
    rn11 <- newNode $ BoxNode 0 () ()
    rn12 <- newNode $ BoxNode 0 () ()
    rn13 <- newNode $ BoxNode 0 () ()
    linkNodes (Ref rn2 1) (Ref rn12 0)
    linkNodes (Ref rn2 2) (Ref rn3 0)
    linkNodes (Ref rn3 1) (Ref rn4 0)
    linkNodes (Ref rn4 1) (Ref rn9 0)
    linkNodes (Ref rn4 2) (Ref rn5 2)
    linkNodes (Ref rn5 0) (Ref rn11 1)
    linkNodes (Ref rn5 1) (Ref rn6 0)
    linkNodes (Ref rn6 1) (Ref rn8 1)
    linkNodes (Ref rn6 2) (Ref rn7 2)
    linkNodes (Ref rn7 0) (Ref rn8 2)
    linkNodes (Ref rn7 1) (Ref rn9 1)
    linkNodes (Ref rn8 0) (Ref rn13 1)
    linkNodes (Ref rn10 1) (Ref rn11 0)
    linkNodes (Ref rn12 1) (Ref rn13 0)
    linkNodes r0 $ Ref rn2 0
    linkNodes r1 $ Ref rn10 0
{-# INLINABLE reassocPure #-}

-- | @r0 = \a -> IOPure (\b -> Bind1C r1 (\c -> Bind0C (a c) b))@
reassocCont :: HasRewriter sig m => Ref -> Ref -> m ()
reassocCont r0 r1 = do
    rn2 <- newNode $ LamNode () () ()
    rn3 <- newNode $ IOPureNode () ()
    rn4 <- newNode $ LamNode () () ()
    rn5 <- newNode $ Bind1CNode () () ()
    rn6 <- newNode $ BoxNode 0 () ()
    rn7 <- newNode $ BoxNode 0 () ()
    rn8 <- newNode $ LamNode () () ()
    rn9 <- newNode $ Bind0CNode () () ()
    rn10 <- newNode $ AppNode () () ()
    rn11 <- newNode $ BoxNode 0 () ()
    rn12 <- newNode $ BoxNode 0 () ()
    rn13 <- newNode $ BoxNode 0 () ()
    linkNodes (Ref rn2 1) (Ref rn11 0)
    linkNodes (Ref rn2 2) (Ref rn3 0)
    linkNodes (Ref rn3 1) (Ref rn4 0)
    linkNodes (Ref rn4 1) (Ref rn13 0)
    linkNodes (Ref rn4 2) (Ref rn5 0)
    linkNodes (Ref rn5 1) (Ref rn7 1)
    linkNodes (Ref rn5 2) (Ref rn8 0)
    linkNodes (Ref rn6 1) (Ref rn7 0)
    linkNodes (Ref rn8 1) (Ref rn10 1)
    linkNodes (Ref rn8 2) (Ref rn9 2)
    linkNodes (Ref rn9 0) (Ref rn10 2)
    linkNodes (Ref rn9 1) (Ref rn13 1)
    linkNodes (Ref rn10 0) (Ref rn12 1)
    linkNodes (Ref rn11 1) (Ref rn12 0)
    linkNodes r0 $ Ref rn2 0
    linkNodes r1 $ Ref rn6 0
{-# INLINABLE reassocCont #-}

mkBranch1 :: HasRewriter sig m => B.Operand -> Ref -> Ref -> Ref -> m ()
mkBranch1 opc r0 r1 r2 = do
    lblt <- newLabel
    lblf <- newLabel
    let b = B.Block mempty $ B.Branch opc lblt lblf
    rn3 <- newNode $ AccumNBNode (B.BlockList b mempty) () ()
    rn4 <- newNode $ Merge0Node () () ()
    rn5 <- newNode $ LabelNode lblt () ()
    rn6 <- newNode $ LabelNode lblf () ()
    rn7 <- newNode $ AccumIONode mempty () ()
    rn8 <- newNode $ AccumIONode mempty () ()
    linkNodes (Ref rn3 0) (Ref rn4 2)
    linkNodes (Ref rn4 0) (Ref rn5 1)
    linkNodes (Ref rn4 1) (Ref rn6 1)
    linkNodes (Ref rn5 0) (Ref rn7 1)
    linkNodes (Ref rn6 0) (Ref rn8 1)
    linkNodes r0 $ Ref rn7 0
    linkNodes r1 $ Ref rn8 0
    linkNodes r2 $ Ref rn3 1
{-# INLINABLE mkBranch1 #-}

mkLambda :: HasRewriter sig m => Ref -> (() -> () -> INetF ()) -> m ()
mkLambda r0 mk1 = do
    rn1 <- newNode $ mk1 () ()
    rn2 <- newNode $ LamNode () () ()
    linkNodes (Ref rn1 0) (Ref rn2 1)
    linkNodes (Ref rn1 1) (Ref rn2 2)
    linkNodes r0 $ Ref rn2 0
{-# INLINE mkLambda #-}

mkSelect :: B.Operand -> B.Operand -> B.Operand -> B.Operand
mkSelect (B.Constant bitc) opt opf = case bitc of
    B.B0 -> opf
    B.B1 -> opt
mkSelect opc (B.Constant B.B1) (B.Constant B.B0) = opc
mkSelect opc opt opf
    | opt == opf = opt
    | otherwise = B.Select opc opt opf
{-# INLINABLE mkSelect #-}

mkChurchBool :: HasRewriter sig m => (m () -> m () -> m ()) -> m Ref
mkChurchBool sel = do
    rn1 <- newNode $ LamNode () () ()
    rn2 <- newNode $ LamNode () () ()
    rn3 <- newNode $ DeadNode ()
    linkNodes (Ref rn1 2) (Ref rn2 0)
    sel (mkTrue rn1 rn2 rn3) (mkFalse rn1 rn2 rn3)
    pure $ Ref rn1 0
  where
    mkTrue rn1 rn2 rn3 = do
        rn4 <- newNode $ BoxNode 0 () ()
        linkNodes (Ref rn2 1) (Ref rn3 0)
        linkNodes (Ref rn1 1) (Ref rn4 0)
        linkNodes (Ref rn2 2) (Ref rn4 1)

    mkFalse rn1 rn2 rn3 = do
        linkNodes (Ref rn1 1) (Ref rn3 0)
        linkNodes (Ref rn2 1) (Ref rn2 2)
{-# INLINABLE mkChurchBool #-}

type HasRewriter sig m = (Has (State INet) sig m, Has (State INetPairs) sig m
    , Has (State INetSize) sig m)

newtype INetSize = INetSize { unINetSize :: Int }
    deriving newtype (Enum, Eq, Num, Ord)

_INetSize :: Iso' INetSize Int
_INetSize = iso unINetSize INetSize
{-# INLINE _INetSize #-}

newNode :: HasRewriter sig m => INetF () -> m Int
newNode mk = do
    idx <- newNodeIndex
    modify $ _INet . at idx ?~ (unsetRef <$ mk)
    pure idx
  where
    unsetRef = Ref (-1) (-1)
{-# INLINE newNode #-}

newName :: HasRewriter sig m => m B.Name
newName = B.Name <$> newNodeIndex
{-# INLINE newName #-}

newLabel :: HasRewriter sig m => m B.Label
newLabel = B.Label <$> newNodeIndex
{-# INLINE newLabel #-}

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

-- | Deletes all 'BoxNode' from the graph. Helps unclutter debugging output.
deleteBoxes :: HasRewriter sig m => m ()
deleteBoxes = do
    size <- gets unINetSize
    go size 0
  where
    go size rn0
        | rn0 < size = do
            net <- gets unINet
            case net IM.!? rn0 of
                Just (BoxNode _ r1 r2) -> do
                    linkNodes r1 r2
                    modify $ _INet . at rn0 .~ Nothing
                _ -> pure ()
            go size $ succ rn0
        | otherwise = pure ()
{-# INLINABLE deleteBoxes #-}

-- | Effect for tracing rewrites.
data TraceRewrite m a where
    {-|
        The @m r@ continuation performs a rewrite on the @'INetF' 'Ref'@ nodes.
        The 'Ref's refer to the principal ports of the nodes.
    -}
    TraceRewrite
        :: Ref -> Ref -> INetF Ref -> INetF Ref -> m r -> TraceRewrite m r

traceRewrite
    :: Has TraceRewrite sig m
    => Ref -> Ref -> INetF Ref -> INetF Ref -> m r -> m r
traceRewrite r0 r1 n0 n1 cont = send $ TraceRewrite r0 r1 n0 n1 cont
{-# INLINE traceRewrite #-}

