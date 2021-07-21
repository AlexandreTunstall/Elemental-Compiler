{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | All AST definitions.
module Language.Elemental.Syntax.Internal
    ( module Language.Elemental.Syntax.Internal
    ) where

import Control.Algebra
import Control.Monad (zipWithM)
import Data.Bifunctor (Bifunctor, bimap, first)
import Data.Fix (Fix, foldFix)
import Data.Functor.Classes (Eq1(liftEq))
import Data.Monoid (All(All), getAll)
import Data.String (IsString)
import Data.Text.Short (ShortText)
import Data.Word (Word32)
import LLVM.AST.Operand (Operand)
import LLVM.AST.Type qualified as LLVM

import Control.Effect.IRBuilder
import Language.Elemental.Rewrite


-- | A program in the AST.
data Program a = Program a [Decl a]

-- | A declaration in the AST.
data Decl a
    -- | A binding: @x = ...@
    = Binding a (DeclName a) (AnnExpr a)
    -- | A foreign import: @foreign import c_func "func" : ...@
    | ForeignImport a (DeclName a) ShortText (AnnType a)
    -- | A foreign export: @foreign export c_func ... : ...@
    | ForeignExport a ShortText (AnnExpr a) (AnnType a)
    -- | A foreign primitive: @foreign primitive primName : ...@
    | ForeignPrimitive a (DeclName a) (AnnType a)
    -- | A foreign address: @foreign address vga_buffer 0xB8000 : ...@
    | ForeignAddress a (DeclName a) Integer (AnnType a)
    deriving stock (Eq)

instance Functor Decl where
    fmap f decl = case decl of
        Binding l dname expr -> Binding (f l) (f <$> dname) (mapExpr f expr)
        ForeignImport l dname foreignName t
            -> ForeignImport (f l) (f <$> dname) foreignName (mapType f t)
        ForeignExport l foreignName expr t
            -> ForeignExport (f l) foreignName (mapExpr f expr) (mapType f t)
        ForeignPrimitive l dname t
            -> ForeignPrimitive (f l) (f <$> dname) (mapType f t)
        ForeignAddress l dname addr t
            -> ForeignAddress (f l) (f <$> dname) addr (mapType f t)

-- | A name in a declaration in the AST.
data DeclName a = DeclName a Name
    deriving stock (Eq, Show, Functor)

-- | An annotated expression in the AST. This is a fixed point of 'AnnExprF'.
type AnnExpr a = Fix (AnnExprF a)

-- | The annotated expression functor.
type AnnExprF a = Biann a ExprF (AnnType a)

-- | An expression in the AST. This is a fixed point of 'ExprF'.
type Expr = Fix (ExprF Type)

-- | The base functor for expressions.
data ExprF t rec
    -- | A reference to a name.
    = Ref Name
    -- | A reference to a variable using its de Bruijn index.
    | Var Int
    -- | Function application. Introduces a variable.
    | App rec rec
    -- | Type function application. Introduces a type variable.
    | TypeApp rec t
    -- | Lambda expression with the given argument type.
    | Lam t rec
    -- | Type lambda expression.
    | TypeLam rec
    -- | Internal expression used for emitting LLVM.
    | InternalExpr (InternalExpr t rec)
    deriving stock (Eq, Show, Foldable, Functor, Traversable)

instance Bimatchable ExprF where
    bimatch (Ref x) (Ref y) = if x == y then pure mempty else mempty
    bimatch (Var x) (Var y) = if x == y then pure mempty else mempty
    bimatch (App x1 x2) (App y1 y2) = (<>) <$> x1 y1 <*> x2 y2
    bimatch (TypeApp x1 x2) (TypeApp y1 y2) = (<>) <$> x1 y1 <*> x2 y2
    bimatch (Lam x1 x2) (Lam y1 y2) = (<>) <$> x1 y1 <*> x2 y2
    bimatch (TypeLam x) (TypeLam y) = x y
    bimatch (InternalExpr ix) (InternalExpr iy) = bimatch ix iy
    bimatch _ _ = mempty

instance Bifunctor ExprF where
    bimap _ _ (Ref x) = Ref x
    bimap _ _ (Var x) = Var x
    bimap _ g (App x y) = App (g x) (g y)
    bimap f g (TypeApp x y) = TypeApp (g x) (f y)
    bimap f g (Lam x y) = Lam (f x) (g y)
    bimap _ g (TypeLam x) = TypeLam (g x)
    bimap f g (InternalExpr ix) = InternalExpr $ bimap f g ix

instance Eq t => Eq1 (ExprF t) where
    liftEq f x y = maybe False getAll $ bimatch (matcher <$> first tmatcher x) y
      where
        matcher x' y' = pure . All $ f x' y'
        tmatcher x' y' = pure . All $ x' == y'

-- | An annotated type in the AST. This is a fixed point of 'AnnTypeF'.
type AnnType a = Fix (Ann a TypeF)

-- | The annotated type functor.
type AnnTypeF a = Ann a TypeF

-- | A type in the AST. This is a fixed point of 'TypeF'.
type Type = Fix TypeF

-- | The base functor for types.
data TypeF rec
    -- | An arrow between two types.
    = Arrow rec rec
    -- | A universally quantified type. Introduces a type variable.
    | Forall rec
    -- | A reference to a type variable using its de Bruijn index.
    | TypeVar Int
    -- | A special type used for the FFI.
    | SpecialType (SpecialType rec)
    deriving stock (Eq, Show, Functor)

instance Eq1 TypeF where
    liftEq f (Arrow x1 x2) (Arrow y1 y2) = f x1 y1 && f x2 y2
    liftEq f (Forall x) (Forall y) = f x y
    liftEq _ (TypeVar x) (TypeVar y) = x == y
    liftEq f (SpecialType x) (SpecialType y) = liftEq f x y
    liftEq _ _ _ = False

instance Matchable TypeF where
    match (Arrow x1 x2) (Arrow y1 y2) = (<>) <$> x1 y1 <*> x2 y2
    match (Forall x) (Forall y) = x y
    match (TypeVar x) (TypeVar y) = if x == y then pure mempty else mempty
    match (SpecialType x) (SpecialType y) = match x y
    match _ _ = mempty

-- | FFI types
data SpecialType rec
    -- | An IO type. This enables side effects with referential transparency.
    = IOType rec
    -- | A pointer type. This enables interacting directly with memory.
    | PointerType PointerKind rec
    -- | An internal type used for emitting LLVM.
    | InternalType (InternalType rec)
    deriving stock (Eq, Show, Functor)

instance Eq1 SpecialType where
    liftEq f (IOType x) (IOType y) = f x y
    liftEq f (PointerType k x) (PointerType k' y) = k == k' && f x y
    liftEq f (InternalType x) (InternalType y) = liftEq f x y
    liftEq _ _ _ = False

instance Matchable SpecialType where
    match (IOType x) (IOType y) = x y
    match (PointerType k x) (PointerType k' y) = if k == k' then x y else mempty
    match (InternalType x) (InternalType y) = match x y
    match _ _ = mempty

-- | The kind of a pointer, indicating which operations are legal on it.
data PointerKind = ReadPointer | WritePointer
    deriving stock (Eq, Show)

-- | Symbol name in the AST.
newtype Name = Name { unName :: ShortText }
    deriving newtype (Eq, IsString, Ord, Show)

{-|
    Internal expressions used for emitting LLVM.
    Front-ends and source parsers should not produce these expressions,
    otherwise strange things may happen.
-}
data InternalExpr t rec
    -- | @pureIO : ∀ 0 -> IO 0@
    = PureIO
    -- | @bindIO : ∀ IO 0 -> ∀ (1 -> IO 0) -> IO 0@
    | BindIO
    {-|
        @purei{n} : i{n} -> IO i{n}@ where @{n}@ is the width of the int (stored
        in the first field).
    -}
    | PurePrim Word32
    {-|
        @bindi{n} : IO (∀ (Bit -> ... -> Bit -> 0) -> 0) -> ∀ (i{n} -> IO 0) ->
        IO 0@ where @{n}@ is the width of the int (stored in the first field) and
        @Bit@ is @∀ 0 -> 0 -> 0@.

        The arguments to the function are stored because this is intended to be
        a valid value after normalising with the emit rules.
    -}
    | BindPrim Word32 rec t rec
    {-|
        Bit vector used to combine multiple @i1@ values into a single @i{n}@
        value where @{n}@ is the length of the first field. The i1 values are
        ordered from LSB to MSB.
        The type of this expression is always @IO i{n}@.
    -}
    | BitVector [rec]
    {-|
        C function call with LLVM-typed arguments.
        The type of this expression is a function type with a return type of
        @IO t@ where @t@ is the user-visible type corresponding to the internal
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
        The type of this expression is always @i{m} -> IO i1@.
    -}
    | IsolateBit Word32 Word32
    -- | @testBit : i1 -> IO (∀ 0 -> 0 -> 0)@
    | TestBit
    -- | @loadPointer : ∀ ReadPointer 0 -> IO 0@
    | LoadPointer
    -- | @storePointer : ∀ WritePointer 0 -> 0 -> IO (∀ 0 -> 0)@
    | StorePointer
    -- | 'StorePointer' except that the stored value is accepted as a primitive.
    | StorePrim LLVM.Type
    {-|
        An LLVM value. This is always of some @IO@ type or a semantically
        equivalent 'InternalType'.
    -}
    | LlvmValue LlvmValue
    | LlvmIO (LlvmGen LlvmValue)
    deriving stock (Eq, Foldable, Functor, Traversable)

instance Bimatchable InternalExpr where
    bimatch PureIO PureIO = pure mempty
    bimatch BindIO BindIO = pure mempty
    bimatch (PurePrim x) (PurePrim y) = if x == y then pure mempty else mempty
    bimatch (BindPrim x xv xt xf) (BindPrim y yv yt yf)
        = if x == y then mconcat [xv yv, xt yt, xf yf] else mempty
    bimatch (BitVector xs) (BitVector ys) = mconcat <$> zipWithM ($) xs ys
    bimatch (Call xop xc xt) (Call yop yc yt)
        = if xop == yop && xc == yc && xt == yt then pure mempty else mempty
    bimatch (IsolateBit ix sx) (IsolateBit iy sy)
        = if ix == iy && sx == sy then pure mempty else mempty
    bimatch TestBit TestBit = pure mempty
    bimatch LoadPointer LoadPointer = pure mempty
    bimatch StorePointer StorePointer = pure mempty
    bimatch (StorePrim x) (StorePrim y) = if x == y then pure mempty else mempty
    bimatch (LlvmValue x) (LlvmValue y) = if x == y then pure mempty else mempty
    -- LlvmIOs never match because IRBuilders are incomparable.
    -- We shouldn't be using Bimatch with these anyway.
    bimatch (LlvmIO _) (LlvmIO _) = mempty
    bimatch _ _ = mempty

instance Bifunctor InternalExpr where
    bimap _ _ PureIO = PureIO
    bimap _ _ BindIO = BindIO
    bimap _ _ (PurePrim s) = PurePrim s
    bimap f g (BindPrim s x t y) = BindPrim s (g x) (f t) (g y)
    bimap _ g (BitVector xs) = BitVector $ g <$> xs
    bimap _ _ (Call op argc t) = Call op argc t
    bimap _ _ (IsolateBit i s) = IsolateBit i s
    bimap _ _ TestBit = TestBit
    bimap _ _ LoadPointer = LoadPointer
    bimap _ _ StorePointer = StorePointer
    bimap _ _ (StorePrim lt) = StorePrim lt
    bimap _ _ (LlvmValue v) = LlvmValue v
    bimap _ _ (LlvmIO gv) = LlvmIO gv


instance Show (InternalExpr t rec) where
    showsPrec _ _ = (<> "<hidden>")

newtype LlvmGen r
    = LlvmGen { genLlvm :: forall sig m. Has IRBuilder sig m => m r }
    deriving stock (Functor)

instance Eq (LlvmGen r) where
    -- We don't need to properly compare these.
    _ == _ = False

data LlvmValue
    -- | @() : i0@
    = LlvmUnit
    -- | Operand of an 'LlvmInt' type of positive non-zero size.
    | LlvmOperand Operand
    deriving stock (Eq)

{-|
    Internal types used for emitting LLVM.
    Front-ends and source parsers should not produce these types.
-}
newtype InternalType rec
    {-|
        An LLVM operand of type @i{n}@, where {n} is the value of the first
        field. In the documentation and the prettyprinter, this is denoted
        identically to the LLVM type for clarity.
    -}
    = LlvmInt Word32
    deriving newtype (Eq, Show)
    deriving stock (Functor)

instance Eq1 InternalType where
    liftEq _ (LlvmInt x) (LlvmInt y) = x == y

instance Matchable InternalType where
    match (LlvmInt x) (LlvmInt y) = if x == y then pure mempty else mempty

-- | Annotated functor.
data Ann a f r = Ann
    { getAnn :: a
    , extract :: f r
    } deriving stock (Functor)

instance Matchable f => Matchable (Ann a f) where
    -- Ignore the annotation because we never want to match it.
    match (Ann _ f) (Ann _ x) = match f x

instance (Eq a, Eq1 f) => Eq1 (Ann a f) where
    liftEq f (Ann a x) (Ann a' y) = a == a' && liftEq f x y

data Biann a f b r = Biann
    { getBiann :: a
    , biextract :: f b r
    } deriving stock (Functor)

instance Bimatchable f => Bimatchable (Biann a f) where
    -- Ignore the annotation because we never want to match it.
    bimatch (Biann _ f) (Biann _ x) = bimatch f x

instance Bifunctor f => Bifunctor (Biann a f) where
    bimap f g (Biann a x) = Biann a $ bimap f g x

instance (Eq a, Eq1 (f b)) => Eq1 (Biann a f b) where
    liftEq f (Biann a x) (Biann a' y) = a == a' && liftEq f x y

-- | Maps the annotation of an 'Ann'.
mapAnn :: (a -> b) -> Ann a f r -> Ann b f r
mapAnn f (Ann a r) = Ann (f a) r

-- | Maps the contents of an 'Ann'.
mapExt :: (f r -> f r') -> Ann a f r -> Ann a f r'
mapExt f (Ann a r) = Ann a $ f r

-- | Maps the annotation of a 'Biann'.
mapBiann :: (a -> b) -> Biann a f g r -> Biann b f g r
mapBiann f (Biann a r) = Biann (f a) r

-- | Maps the contents of a 'Biann'.
mapBiext :: (f g r -> f g' r') -> Biann a f g r -> Biann a f g' r'
mapBiext f (Biann a r) = Biann a $ f r

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
        ForeignAddress l _ _ _ -> l

instance Labelled DeclName where
    getLabel dname = case dname of
        DeclName l _ -> l

-- | Gets all the referenced names in an annotated expression.
getReferences :: AnnExpr a -> [(a, Name)]
getReferences = foldFix $ \(Biann l ea) -> case ea of
    Ref ref -> [(l, ref)]
    Var {} -> []
    App ef ex -> ef <> ex
    TypeApp ef _ -> ef
    Lam _ ex -> ex
    TypeLam ex -> ex
    InternalExpr iex -> case iex of
        PureIO {} -> []
        BindIO {} -> []
        PurePrim {} -> []
        BindPrim {} -> []
        BitVector es -> concat es
        Call {} -> []
        IsolateBit {} -> []
        TestBit {} -> []
        LoadPointer {} -> []
        StorePointer {} -> []
        StorePrim {} -> []
        LlvmValue {} -> []
        LlvmIO {} -> []

-- | Strips the annotations from an 'AnnExpr', turning it into an 'Expr'.
stripExpr :: AnnExpr a -> Expr
stripExpr = mapFix $ first stripType . biextract

-- | Adds an annotation to an 'Expr', turning it into an 'AnnExpr'.
annExpr :: a -> Expr -> AnnExpr a
annExpr l = mapFix $ Biann l . first (annType l)

-- | Strips the annotation from an 'AnnType', turning it into a 'Type'.
stripType :: AnnType a -> Type
stripType = mapFix extract

-- | Adds an annotation to a 'Type', turning it into an 'AnnType'.
annType :: a -> Type -> AnnType a
annType = mapFix . Ann

-- | Maps the annotation on an 'AnnExpr'.
mapExpr :: (a -> b) -> AnnExpr a -> AnnExpr b
mapExpr f = mapFix $ mapBiann f . mapBiext (first $ mapType f)

-- | Maps the annotation on an 'AnnType'.
mapType :: (a -> b) -> AnnType a -> AnnType b
mapType = mapFix . mapAnn
