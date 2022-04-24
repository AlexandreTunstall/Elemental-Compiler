{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Elemental.Backend
    ( Name(..)
    , Named(..)
    , Type(..)
    , Bit(..)
    , Operand(..)
    , Instruction(..)
    , Body(..)
    , Function(..)
    , External(..)
    , Program(..)
    , opType
    , instrType
    , concatBody
    -- * Partial
    , Partial(..)
    , FoldArrow
    , addOperand
    -- * Renaming
    , renameOp
    , renameInstr
    , renameBody
    -- * Traversals
    , opRefs
    , instrOps
    , instrBodies
    , bodyOps
    , bodyBoundNames
    , bodyFreeRefs
    ) where

import Control.Lens (Traversal', anyOf, (%~))
import Data.ByteString.Short (ShortByteString)
import Data.DList (DList, snoc, toList)
import Data.Foldable (foldl')
import Data.Kind qualified as Kind
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))
import Numeric.Natural (Natural)
import Prettyprinter
    ( Doc, Pretty(pretty)
    , concatWith, encloseSep, flatAlt, group
    , hardline, line, nest, parens, tupled
    , (<+>)
    )

import Language.Elemental.Singleton

-- | Names for the operand namespace and the function namespace.
data Name
    = Name Integer
    | ExternalName ShortByteString
    | SubName Name Integer
    | UnusedName
    deriving stock (Eq, Ord, Read, Show)

instance IsString Name where
    fromString = ExternalName . fromString

instance Pretty Name where
    pretty (Name n) = pretty n
    pretty (ExternalName str) = pretty $ show str
    pretty (SubName name n) = pretty name <> "." <> pretty n
    pretty UnusedName = "_"

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
    -- pretty (Reference t name) = pretty t <+> "%" <> pretty name
    -- pretty (Address t addr) = pretty t <+> "@" <> pretty addr
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
    -- | Calls the symbol 'Name' with the given arguments.
    | Call Type Name [Operand]
    -- | Loads the value of a pointer.
    | Load Operand
    -- | Stores the second value into the first pointer.
    | Store Operand Operand
    {-|
        Runs the instructions in the first body if the value of the operand is
        @True@. Otherwise, it runs the instructions in the second body.
    -}
    | Branch Operand Body Body
    deriving stock (Eq, Read, Show)

instance Pretty Instruction where
    pretty (Pure op) = "Pure" <+> pretty op
    pretty (Call _ name args)
        = foldl' (<+>) ("Call" <+> pretty name) (pretty <$> args)
    pretty (Load op) = "Load" <+> pretty op
    pretty (Store op1 op2) = "Store" <+> pretty op1 <+> pretty op2
    pretty (Branch op bt bf) = nest 4 ("If" <+> pretty op >>> pretty bt)
        >>> nest 4 ("Else" >>> pretty bf) >>> "End"

instrType :: Instruction -> Type
instrType = \case
    Pure op -> opType op
    Call t _ _ -> t
    Load ptr -> opType ptr
    Store _ _ -> IntType 0
    Branch _ bt _ -> instrType $ bodyTerm bt

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

data Named a = Name := a
    deriving stock (Eq, Read, Show, Foldable, Functor, Traversable)

instance Pretty a => Pretty (Named a) where
    pretty (name := a) = pretty name <+> "=" <+> pretty a

data Body = Body
    { bodyInstrs :: DList (Named Instruction)
    -- | The last instruction in the body.
    , bodyTerm :: Instruction
    } deriving stock (Eq, Read, Show)

instance Pretty Body where
    pretty b = concatWith (>>>) . toList
        $ snoc (pretty <$> bodyInstrs b) (pretty $ bodyTerm b)

data Function = Function
    { functionArgs :: [Named Type]
    , functionBody :: Body
    } deriving stock (Eq, Read, Show)

instance Pretty Function where
    pretty f = nest 4 ("Function" <+> tupled (pretty <$> functionArgs f)
        >>> pretty (functionBody f))
        >>> "End"

data External = External
    { externalArgs :: [Type]
    , externalRet :: Type
    } deriving stock (Eq, Read, Show)

instance Pretty External where
    pretty (External args ret) = pretty ret <+> tupled (pretty <$> args)

data Program = Program
    { programImports :: [Named External]
    , programFunctions :: [Named Function] }
    deriving stock (Eq, Read, Show)

instance Pretty Program where
    pretty (Program exts funcs) = concatWith f
        [ concatWith f (pretty <$> exts)
        , concatWith f (pretty <$> funcs)
        ]
      where
        f a b = a <> line <> line <> b

addOperand :: Operand -> Partial a -> Either (Partial a) a
addOperand op (Partial (SSucc n) f) = case n of
    SZero -> Right $ f op
    SSucc _ -> Left $ Partial n $ f op

concatBody :: Name -> Body -> Body -> Body
concatBody name b1 b2 = Body
    { bodyInstrs = snoc (bodyInstrs b1) (name := bodyTerm b1) <> bodyInstrs b2
    , bodyTerm = bodyTerm b2
    }

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

renameOp :: M.Map Name Operand -> Operand -> Operand
renameOp re op = case op of
    Reference _ name -> fromMaybe op $ re M.!? name
    Address _ _ -> op
    Empty -> op
    Constant _ -> op
    IsolateBit size idx op' -> IsolateBit size idx $ renameOp re op'
    InsertBit size oph opt -> InsertBit size (renameOp re oph) (renameOp re opt)
    Select opc opt opf
        -> Select (renameOp re opc) (renameOp re opt) (renameOp re opf)
    Tuple ops -> Tuple $ renameOp re <$> ops
    GetElement idx opt -> GetElement idx $ renameOp re opt

renameInstr :: M.Map Name Operand -> Instruction -> Instruction
renameInstr re = instrOps %~ renameOp re

instrOps :: Traversal' Instruction Operand
instrOps f instr = case instr of
    Pure op -> Pure <$> f op
    Call t name args -> Call t name <$> traverse f args
    Load ptr -> Load <$> f ptr
    Store ptr op -> Store <$> f ptr <*> f op
    Branch opc bt bf -> Branch <$> f opc <*> bodyOps f bt <*> bodyOps f bf

instrBodies :: Traversal' Instruction Body
instrBodies f instr = case instr of
    Pure _ -> pure instr
    Call {} -> pure instr
    Load _ -> pure instr
    Store _ _ -> pure instr
    Branch opc bt bf -> Branch opc <$> f bt <*> f bf

renameBody :: M.Map Name Operand -> Body -> Body
renameBody re = bodyOps %~ renameOp re

bodyOps :: Traversal' Body Operand
bodyOps f (Body instrs term) = Body <$> go instrs <*> instrOps f term
  where
    go = traverse . traverse $ instrOps f

bodyBoundNames :: Traversal' Body Name
bodyBoundNames f (Body instrs term) = Body <$> go instrs <*> visit term
  where
    go = traverse $ \(name := instr) -> (:=) <$> f name <*> visit instr
    visit = instrBodies . bodyBoundNames $ f

bodyFreeRefs :: Traversal' Body (Type, Name)
bodyFreeRefs f b@(Body instrs term) = Body <$> go instrs <*> instrNames g term
  where
    go = traverse . traverse $ instrNames g
    g ref
        | anyOf bodyBoundNames (== snd ref) b = pure ref
        | otherwise = f ref

    instrNames :: Traversal' Instruction (Type, Name)
    instrNames = instrOps . opRefs

(>>>) :: Doc ann -> Doc ann -> Doc ann
a >>> b = a <> flatAlt hardline "; " <> b
infixr 6 >>>

braced :: [Doc ann] -> Doc ann
braced = group . encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "

