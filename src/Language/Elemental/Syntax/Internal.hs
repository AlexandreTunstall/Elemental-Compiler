{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | All AST definitions.
module Language.Elemental.Syntax.Internal
    ( module Language.Elemental.Syntax.Internal
    ) where

import Control.Algebra
import Data.String (IsString)
import Data.Text.Short (ShortText)
import Data.Word (Word32)
import LLVM.AST.Operand (Operand)
import LLVM.AST.Type qualified as LLVM

import Control.Effect.IRBuilder
import Language.Elemental.Rewrite


-- | A program in the AST.
data Program a = Program a [Decl a]
    deriving stock (Show, Functor)

-- | A declaration in the AST.
data Decl a
    -- | A binding: @x = ...@
    = Binding a (DeclName a) (Expr a)
    -- | A foreign import: @foreign import c_func "func" : ...@
    | ForeignImport a (DeclName a) ShortText (Type a)
    -- | A foreign export: @foreign export c_func ... : ...@
    | ForeignExport a ShortText (Expr a) (Type a)
    -- | A foreign primitive: @foreign primitive primName : ...@
    | ForeignPrimitive a (DeclName a) (Type a)
    deriving stock (Show, Functor)

-- | A name in a declaration in the AST.
data DeclName a = DeclName a Name
    deriving stock (Show, Functor)

-- | An expression in the AST.
data Expr a
    -- | A reference to a name.
    = Ref a Name
    -- | A reference to a variable using its de Bruijn index.
    | Var a Int
    -- | Function application. Introduces a variable.
    | App a (Expr a) (Expr a)
    -- | Type function application. Introduces a type variable.
    | TypeApp a (Expr a) (Type a)
    -- | Lambda expression with the given argument type.
    | Lam a (Type a) (Expr a)
    -- | Type lambda expression.
    | TypeLam a (Expr a)
    -- | Internal expression used for emitting LLVM.
    | InternalExpr a (InternalExpr a)
    deriving stock (Show, Functor)

-- | A type in the AST.
data Type a
    -- | An arrow between two types.
    = Arrow a (Type a) (Type a)
    -- | A universally quantified type. Introduces a type variable.
    | Forall a (Type a)
    -- | A reference to a type variable using its de Bruijn index.
    | TypeVar a Int
    -- | A special type used for the FFI.
    | SpecialType a (SpecialType a)
    deriving stock (Eq, Show, Functor)

-- | FFI types
data SpecialType a
    -- | An IO type. This enables side effects with referential transparency.
    = IOType a (Type a)
    -- | An internal type used for emitting LLVM.
    | InternalType a (InternalType a)
    deriving stock (Eq, Show, Functor)

-- | Symbol name in the AST.
newtype Name = Name { unName :: ShortText }
    deriving newtype (Eq, IsString, Ord, Show)

{-|
    Internal expressions used for emitting LLVM.
    Front-ends and source parsers should not produce these expressions,
    otherwise strange things may happen.
-}
data InternalExpr a
    -- | @() : IO (∀ 0 -> 0)@
    = Unit
    -- | @pureIO : ∀ 0 -> IO 0@
    | PureIO
    -- | @bindIO : ∀ IO 0 -> ∀ (1 -> IO 0) -> IO 0@
    | BindIO
    {-|
        @purei{n} : i{n} -> IO i{n}@ where @{n}@ is the width of the int (stored
        in the first field).
    -}
    | PurePrim Word32
    {-|
        @bindi{n} : IO (∀ (Bit -> ... -> Bit -> 0) -> 0) -> ∀ (i# -> IO 0) -> IO
        0@ where @{n}@ is the width of the int (stored in the first field) and
        @Bit@ is @∀ 0 -> 0 -> 0@.
    -}
    | BindPrim Word32
    {-|
        Bit vector used to combine multiple @i1@ values into a single @i{n}@
        value where @{n}@ is the length of the first field. The i1 values are
        ordered from LSB to MSB.
        The type of this expression is always @i{n}@.
    -}
    | BitVector [Expr a]
    {-|
        C function call with LLVM-typed arguments.
        The type of this expression is a function type with a return type of @IO
        t@ where @t@ is the user-visible type corresponding to the internal
        return type.
    -}
    | Call Operand Int LLVM.Type
    {-|
        Function that isolates the nth (stored in the first field) LSB from an
        @i{m}@ where @{m}@ is the value of the second field. In other words,
        @'IsolateBit' 3 8@ on the operand @%v@ could be compiled into:
        
        > %v.3t = shr i8 %v, 3
        > %v.3 = trunc i8 %v.3t to i1
        
        to produce the operand @%v.3@.
        The type of this expression is always @i{m} -> i1@.
    -}
    | IsolateBit Word32 Word32
    -- | @testBit : i1 -> ∀ 0 -> 0 -> 0@
    | TestBit
    {-|
        An LLVM operand. Usually of an IO type, an 'InternalType' type, or a
        function type.
    -}
    | LlvmOperand Operand
    -- | Emits LLVM code with the expression.
    | Emit (forall sig m. (Has IRBuilder sig m, Has (Rewriter (Expr a)) sig m)
        => m (Expr a))

{-
    Functor can't be automatically derived because of the @Rewriter (Expr a)@ in
    the 'Emit' constructor.
-}
instance Functor InternalExpr where
    fmap f ie = case ie of
        Unit -> Unit
        PureIO -> PureIO
        BindIO -> BindIO
        PurePrim size -> PurePrim size
        BindPrim size -> BindPrim size
        BitVector es -> BitVector $ fmap f <$> es
        Call op argc tret -> Call op argc tret
        IsolateBit bit size -> IsolateBit bit size
        TestBit -> TestBit
        LlvmOperand op -> LlvmOperand op
        Emit m -> Emit $ mapExprRewriter f m

-- | Maps the label of an expression rewritter.
mapExprRewriter
    :: Has (Rewriter (Expr b)) sig m => (a -> b)
    -> (forall n. Algebra (Rewriter (Expr a) :+: sig) n => n (Expr a))
    -> m (Expr b)
mapExprRewriter f = runRewriter (pure . fmap f) (mapTrack f) (mapLocal f)
  where
    mapTrack :: Has (Rewriter (Expr b)) sig m => (a -> b) -> Expr a -> m ()
    mapTrack g = trackRewrite . fmap g

    mapLocal :: Has (Rewriter (Expr b)) sig m => (a -> b) -> Expr a -> m s -> m s
    mapLocal g = localRewrite . fmap g

{-|
    Internal types used for emitting LLVM.
    Front-ends and source parsers should not produce these types.
-}
newtype InternalType a
    {-|
        An LLVM operand of type @i{n}@, where {n} is the value of the first
        field. In the documentation and the prettyprinter, this is denoted
        identically to the LLVM type for clarity.
    -}
    = LlvmInt Word32
    deriving newtype (Eq, Show)
    deriving stock (Functor)

instance Show (InternalExpr a) where
    showsPrec _ _ = (<> "<hidden>")

-- | Class used for labelled types.
class Labelled f where
    -- | Gets the label for the given value.
    getLabel :: f a -> a

instance Labelled Program where
    getLabel p = case p of
        Program l _ -> l

instance Labelled Decl where
    getLabel decl = case decl of
        Binding l _ _ -> l
        ForeignImport l _ _ _ -> l
        ForeignExport l _ _ _ -> l
        ForeignPrimitive l _ _ -> l

instance Labelled DeclName where
    getLabel dname = case dname of
        DeclName l _ -> l

instance Labelled Expr where
    getLabel expr = case expr of
        Ref l _ -> l
        Var l _ -> l
        App l _ _ -> l
        TypeApp l _ _ -> l
        Lam l _ _ -> l
        TypeLam l _ -> l
        InternalExpr l _ -> l

instance Labelled Type where
    getLabel t = case t of
        Arrow l _ _ -> l
        Forall l _ -> l
        TypeVar l _ -> l
        SpecialType l _ -> l

instance Labelled SpecialType where
    getLabel st = case st of
        IOType l _ -> l
        InternalType l _ -> l

-- | Unidirectional operator synonym for 'App'.
pattern (:$) :: Expr a -> Expr a -> Expr a
pattern ef :$ ex <- App _ ef ex
infixl 1 :$

-- | Unidirectional operator synonym for 'TypeApp'.
pattern (:@) :: Expr a -> Type a -> Expr a
pattern ef :@ tx <- TypeApp _ ef tx
infixl 1 :@

-- | Unidirectional operator synonym for 'Lam'.
pattern (:\) :: Type a -> Expr a -> Expr a
pattern tx :\ ey <- Lam _ tx ey
infixr 0 :\

-- | Unidirectional operator synonym for 'Arrow'.
pattern (:->) :: Type a -> Type a -> Type a
pattern tx :-> ty <- Arrow _ tx ty
infixr 2 :->

-- | Gets the list of names referenced in the given expression.
getReferences :: Expr a -> [(a, Name)]
getReferences ea = case ea of
    Ref l ref -> [(l, ref)]
    Var {} -> []
    App _ ef ex -> getReferences ef <> getReferences ex
    TypeApp _ ef _ -> getReferences ef
    Lam _ _ ex -> getReferences ex
    TypeLam _ ex -> getReferences ex
    InternalExpr _ iex -> case iex of
        Unit {} -> []
        PureIO {} -> []
        BindIO {} -> []
        PurePrim {} -> []
        BindPrim {} -> []
        BitVector es -> concatMap getReferences es
        Call {} -> []
        IsolateBit {} -> []
        TestBit -> []
        LlvmOperand {} -> []
        -- Let's pretend this doesn't contain an expression.
        -- This should be fine as long as frontends and parsers don't misbehave.
        Emit _ -> []
