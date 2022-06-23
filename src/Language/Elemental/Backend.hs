{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Elemental.Backend
    ( Name(..)
    , Named
    , FunctionName(..)
    , NamedFunction
    , ForeignName(..)
    , ForeignNamed
    , Named'(..)
    , Label(..)
    , Type(..)
    , Bit(..)
    , Operand(..)
    , Instruction(..)
    , Terminator(..)
    , _Return
    , _TailCall
    , Block(..)
    , _entryBlock
    , _namedBlocks
    , IBlock(..)
    , _IBlock
    , BlockList(..)
    , _blockInstrs
    , _blockTerm
    , NamedBlockList(..)
    , Function(..)
    , _functionArgs
    , _functionRet
    , ImplicitFunction(..)
    , _ifunctionBlocks
    , External(..)
    , Program(..)
    , opType
    , instrType
    -- * Partial
    , Partial(..)
    , FoldArrow
    , addOperand
    -- * Traversals
    , opRefs
    , instrOps
    , termOps
    , blockOps
    , blockBoundNames
    , blockFreeRefs
    , blockListBlocks
    , blockListBoundNames
    , blockListFreeRefs
    ) where

import Control.Lens
    ( Bifunctor, Iso', Lens', Prism', Traversal'
    , anyOf, bimap, filtered, iso, lens, noneOf, prism
    )
import Data.ByteString.Short (ShortByteString)
import Data.DList (DList, snoc, toList)
import Data.Foldable (foldl')
import Data.IntMap qualified as IM
import Data.Kind qualified as Kind
import Data.String (IsString(fromString))
import Numeric.Natural (Natural)
import Prettyprinter
    ( Doc, Pretty(pretty)
    , concatWith, encloseSep, flatAlt, group
    , hardline, indent, line, nest, parens, tupled
    , (<+>)
    )

import Language.Elemental.Singleton

-- | Names for the operand namespace.
newtype Name = Name Int
    deriving stock (Eq, Ord, Read, Show)
    deriving newtype (Pretty)

-- | Names for the function namespace.
data FunctionName = PrivateName Name | ExternalName ForeignName
    deriving stock (Eq, Ord, Read, Show)

instance Pretty FunctionName where
    pretty (PrivateName n) = pretty n
    pretty (ExternalName n) = pretty n

newtype ForeignName = ForeignName { unForeignName :: ShortByteString }
    deriving stock (Eq, Ord, Read, Show)

instance IsString ForeignName where
    fromString = ForeignName . fromString

instance Pretty ForeignName where
    pretty = pretty . show . unForeignName

newtype Label = Label { unLabel :: Int }
    deriving newtype (Eq, Ord, Read, Show)

instance Pretty Label where
    pretty (Label idx) = "@" <> pretty idx

data Type = IntType Int | TupleType [Type]
    deriving stock (Eq, Read, Show)

instance Pretty Type where
    pretty (IntType size) = "i" <> pretty size
    pretty (TupleType ts) = braced $ pretty <$> ts

data Bit = B0 | B1
    deriving stock (Enum, Eq, Read, Show)

instance Pretty Bit where
    pretty B0 = "0"
    pretty B1 = "1"

data Operand
    = Reference Type Name
    | Address Type Natural
    | Empty
    | Constant Bit
    -- | Extracts the nth LSB.
    | IsolateBit Int Int Operand
    -- | Inserts a new MSB.
    | InsertBit Int Operand Operand
    {-|
        The first operand is the condition. The second operand is the result
        when the condition's value is @True@.
    -}
    | Select Operand Operand Operand
    | Tuple [Operand]
    | GetElement Int Operand
    deriving stock (Eq, Read, Show)

instance Pretty Operand where
    pretty (Reference _ name) = "%" <> pretty name
    pretty (Address _ addr) = "@" <> pretty addr
    pretty Empty = "[]"
    pretty (Constant b) = "#" <> pretty b
    pretty (IsolateBit _ idx op) = parens $ pretty op <+> "!!" <+> pretty idx
    pretty (InsertBit _ b op) = parens $ pretty b <+> ":" <+> pretty op
    pretty (Select op opt opf) = parens $ "if" <+> pretty op
        <+> "then" <+> pretty opt <+> "else" <+> pretty opf
    pretty (Tuple ops) = braced $ pretty <$> ops
    pretty (GetElement idx op) = parens $ pretty op <+> "!" <+> pretty idx

opType :: Operand -> Type
opType = \case
    Reference t _ -> t
    Address t _ -> t
    Empty -> IntType 0
    Constant _ -> IntType 1
    IsolateBit {} -> IntType 1
    InsertBit size _ _ -> IntType $ succ size
    Select _ opt _ -> opType opt
    Tuple ops -> TupleType $ opType <$> ops
    GetElement idx op -> case opType op of
        TupleType ts | length ts > idx -> ts !! idx
        _ -> error "illegal GetElement"

data Instruction
    -- | Allows setting a = b.
    = Pure Operand
    -- | Calls the function with the given arguments.
    | Call Type FunctionName [Operand]
    -- | Loads the value of a pointer.
    | Load Operand
    -- | Stores the second value into the first pointer.
    | Store Operand Operand
    deriving stock (Eq, Read, Show)

instance Pretty Instruction where
    pretty (Pure op) = "Pure" <+> pretty op
    pretty (Call _ name args)
        = foldl' (<+>) ("Call" <+> pretty name) (pretty <$> args)
    pretty (Load op) = "Load" <+> pretty op
    pretty (Store op1 op2) = "Store" <+> pretty op1 <+> pretty op2

instrType :: Instruction -> Type
instrType = \case
    Pure op -> opType op
    Call t _ _ -> t
    Load ptr -> opType ptr
    Store _ _ -> IntType 0

data Terminator
    = Jump Label
    {-|
        Jumps to the first label if the value of the operand is @True@.
        Otherwise, it jumps to the second label.
    -}
    | Branch Operand Label Label
    | Return Operand
    -- | Calls the function and returns its return value.
    | TailCall Name Operand
    -- | Indicates that this block is unreachable.
    | Unreachable
    deriving stock (Eq, Read, Show)

instance Pretty Terminator where
    pretty (Jump lbl) = "Jump" <+> pretty lbl
    pretty (Branch op lblt lblf)
        = "Branch" <+> pretty op <+> pretty lblt <+> pretty lblf
    pretty (Return op) = "Return" <+> pretty op
    pretty (TailCall name parg) = "TailCall" <+> pretty name <+> pretty parg
    pretty Unreachable = "Unreachable"

_Return :: Prism' Terminator Operand
_Return = prism Return $ \case
    Return op -> Right op
    term -> Left term
{-# INLINE _Return #-}

_TailCall :: Prism' Terminator (Name, Operand)
_TailCall = prism (uncurry TailCall) $ \case
    TailCall name parg -> Right (name, parg)
    term -> Left term
{-# INLINE _TailCall #-}

data Partial a where
    Partial :: SNat ('Succ n) -> FoldArrow ('Succ n) Operand a -> Partial a

-- Doesn't check equality of the arrows; for testing.
instance Eq (Partial a) where
    Partial a _ == Partial b _ = go a b
      where
        go :: SNat a -> SNat b -> Bool
        go SZero SZero = True
        go (SSucc _) SZero = False
        go SZero (SSucc _) = False
        go (SSucc a') (SSucc b') = go a' b'

instance Functor Partial where
    fmap f' (Partial arity g') = Partial arity $ foldNat f' arity g'
      where
        foldNat
            :: (a -> b) -> SNat n
            -> FoldArrow n Operand a -> FoldArrow n Operand b
        foldNat f SZero g = f g
        foldNat f (SSucc n) g = foldNat f n . g

instance Pretty (Partial a) where
    pretty (Partial n _) = "p" <> pretty (toNatural n)

type FoldArrow :: Nat -> Kind.Type -> Kind.Type -> Kind.Type
type family FoldArrow n a b where
    FoldArrow 'Zero _ b = b
    FoldArrow ('Succ n) a b = a -> FoldArrow n a b

data Named' a b = a := b
    deriving stock (Eq, Read, Show, Foldable, Functor, Traversable)

instance (Pretty a, Pretty b) => Pretty (Named' a b) where
    pretty (name := a) = pretty name <+> "=" <+> pretty a

instance Bifunctor Named' where
    bimap f g (name := a) = f name := g a

type Named = Named' Name
type ForeignNamed = Named' ForeignName
type NamedFunction
    = Either (Named ImplicitFunction) (Named' FunctionName Function)

data Block = Block
    { blockInstrs :: DList (Named Instruction)
    , blockTerm :: Terminator
    } deriving stock (Eq, Read, Show)

instance Pretty Block where
    pretty b = concatWith (>>>) . toList
        $ snoc (pretty <$> blockInstrs b) (pretty $ blockTerm b)

_blockInstrs :: Lens' Block (DList (Named Instruction))
_blockInstrs = lens blockInstrs $ \b instrs -> b { blockInstrs = instrs }
{-# INLINE _blockInstrs #-}

_blockTerm :: Lens' Block Terminator
_blockTerm = lens blockTerm $ \b term -> b { blockTerm = term }
{-# INLINE _blockTerm #-}

newtype IBlock = IBlock { unIBlock :: DList (Named Instruction) }
    deriving stock (Eq, Read, Show)
    deriving newtype (Monoid, Semigroup)

instance Pretty IBlock where
    pretty (IBlock instrs) = concatWith (>>>) . toList $ pretty <$> instrs

_IBlock :: Iso' IBlock (DList (Named Instruction))
_IBlock = iso unIBlock IBlock
{-# INLINE _IBlock #-}

data BlockList = BlockList
    { entryBlock :: Block
    , namedBlocks :: NamedBlockList
    } deriving stock (Eq, Read, Show)

instance Pretty BlockList where
    pretty bs = concatWith (>>>)
        $ indent 4 (pretty $ entryBlock bs) : prettyNamedBlocks (namedBlocks bs)

_entryBlock :: Lens' BlockList Block
_entryBlock = lens entryBlock $ \bs b -> bs { entryBlock = b }
{-# INLINE _entryBlock #-}

_namedBlocks :: Lens' BlockList NamedBlockList
_namedBlocks = lens namedBlocks $ \bs nbs -> bs { namedBlocks = nbs }
{-# INLINE _namedBlocks #-}

newtype NamedBlockList = NamedBlockList { unNamedBlockList :: IM.IntMap Block }
    deriving stock (Eq, Read, Show)
    deriving newtype (Monoid, Semigroup)

instance Pretty NamedBlockList where
    pretty nbs = concatWith (>>>) $ prettyNamedBlocks nbs

prettyNamedBlocks :: NamedBlockList -> [Doc ann]
prettyNamedBlocks nbs = prettyLabel <$> IM.assocs (unNamedBlockList nbs)
  where
    prettyLabel (lbl, b) = nest 4 $ pretty lbl <> ":" <> line <> pretty b

data Function = Function
    { functionArgs :: [Named Type]
    , functionRet :: Type
    , functionBlocks :: BlockList
    } deriving stock (Eq, Read, Show)

instance Pretty Function where
    pretty f = "Function" <+> pretty (functionRet f)
        <+> tupled (pretty <$> functionArgs f)
        >>> pretty (functionBlocks f)
        >>> "End"

_functionArgs :: Lens' Function [Named Type]
_functionArgs = lens functionArgs $ \f args -> f { functionArgs = args }
{-# INLINE _functionArgs #-}

_functionRet :: Lens' Function Type
_functionRet = lens functionRet $ \f t -> f { functionRet = t }
{-# INLINE _functionRet #-}

data ImplicitFunction = ImplicitFunction
    { ifunctionArgs :: [Named Type]
    , ifunctionBlocks :: BlockList
    }
    deriving stock (Eq, Read, Show)

instance Pretty ImplicitFunction where
    pretty f = "Implicit Function" <+> tupled (pretty <$> ifunctionArgs f)
        >>> pretty (ifunctionBlocks f) >>> "End"

_ifunctionBlocks :: Lens' ImplicitFunction BlockList
_ifunctionBlocks = lens ifunctionBlocks $ \f bs -> f { ifunctionBlocks = bs }
{-# INLINE _ifunctionBlocks #-}

data External = External
    { externalArgs :: [Type]
    , externalRet :: Type
    } deriving stock (Eq, Read, Show)

instance Pretty External where
    pretty (External args ret) = pretty ret <+> tupled (pretty <$> args)

data Program = Program
    { programImports :: [ForeignNamed External]
    , programFunctions :: [NamedFunction]
    }
    deriving stock (Eq, Read, Show)

instance Pretty Program where
    pretty (Program exts funcs)
        = concatWith f $ (pretty <$> exts) <> (either pretty pretty <$> funcs)
      where
        f a b = a <> line <> line <> b

addOperand :: Operand -> Partial a -> Either (Partial a) a
addOperand op (Partial (SSucc n) f) = case n of
    SZero -> Right $ f op
    SSucc _ -> Left $ Partial n $ f op

opRefs :: Traversal' Operand (Type, Name)
opRefs f op = case op of
    Reference t name -> uncurry Reference <$> f (t, name)
    Address _ _ -> pure op
    Empty -> pure op
    Constant _ -> pure op
    IsolateBit size idx op' -> IsolateBit size idx <$> opRefs f op'
    InsertBit size oph opt -> InsertBit size <$> opRefs f oph <*> opRefs f opt
    Select opc opt opf
        -> Select <$> opRefs f opc <*> opRefs f opt <*> opRefs f opf
    Tuple ops -> Tuple <$> traverse (opRefs f) ops
    GetElement idx opt -> GetElement idx <$> opRefs f opt
{-# INLINABLE opRefs #-}

instrOps :: Traversal' Instruction Operand
instrOps f instr = case instr of
    Pure op -> Pure <$> f op
    Call t name args -> Call t name <$> traverse f args
    Load ptr -> Load <$> f ptr
    Store ptr op -> Store <$> f ptr <*> f op

termOps :: Traversal' Terminator Operand
termOps f = \case
    Jump lbl -> pure $ Jump lbl
    Branch opc lblt lblf -> (\opc' -> Branch opc' lblt lblf) <$> f opc
    Return op -> Return <$> f op
    TailCall name parg -> TailCall name <$> f parg
    Unreachable -> pure Unreachable
{-# INLINABLE termOps #-}

blockOps :: Traversal' Block Operand
blockOps f (Block instrs term) = Block <$> go instrs <*> termOps f term
  where
    go = traverse . traverse $ instrOps f
{-# INLINABLE blockOps #-}

blockBoundNames :: Traversal' Block Name
blockBoundNames f (Block instrs term) = Block <$> go instrs <*> pure term
  where
    go = traverse $ \(name := instr) -> (:=) <$> f name <*> pure instr
{-# INLINABLE blockBoundNames #-}

blockFreeRefs :: Traversal' Block (Type, Name)
blockFreeRefs f b@(Block instrs term) = Block <$> go instrs <*> termNames g term
  where
    go = traverse . traverse $ instrNames g
    g ref
        | anyOf blockBoundNames (== snd ref) b = pure ref
        | otherwise = f ref

    instrNames :: Traversal' Instruction (Type, Name)
    instrNames = instrOps . opRefs

    termNames :: Traversal' Terminator (Type, Name)
    termNames = termOps . opRefs
{-# INLINABLE blockFreeRefs #-}

blockListBlocks :: Traversal' BlockList Block
blockListBlocks f (BlockList eb (NamedBlockList nbs))
    = BlockList <$> f eb <*> (NamedBlockList <$> traverse f nbs)
{-# INLINABLE blockListBlocks #-}

blockListBoundNames :: Traversal' BlockList Name
blockListBoundNames = blockListBlocks . blockBoundNames
{-# INLINABLE blockListBoundNames #-}

blockListFreeRefs :: Traversal' BlockList (Type, Name)
blockListFreeRefs f bs = (blockListBlocks . blockFreeRefs . filtered g) f bs
  where
    g :: (Type, Name) -> Bool
    g (_, name) = noneOf blockListBoundNames (name ==) bs
{-# INLINABLE blockListFreeRefs #-}

(>>>) :: Doc ann -> Doc ann -> Doc ann
a >>> b = a <> flatAlt hardline "; " <> b
infixr 6 >>>

braced :: [Doc ann] -> Doc ann
braced = group . encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "

