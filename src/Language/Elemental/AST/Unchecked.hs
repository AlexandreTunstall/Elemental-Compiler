{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Language.Elemental.AST.Unchecked
    ( UProgramF(..)
    , UDeclF(..)
    , mapDeclF
    , UExprF(..)
    , UTypeF(..)
    , Name(..)
    -- * Pure Fixed Points
    , UProgram
    , UDecl
    , UExpr
    , UType
    ) where

import Data.Bifoldable (Bifoldable(bifoldMap))
import Data.Bifunctor (Bifunctor(bimap))
import Data.Data (Data)
import Data.Fix (Fix)
import Data.Functor.Classes (Eq1(liftEq))
import Data.String (IsString)
import Data.Text.Short (ShortText, toText)
import Numeric.Natural (Natural)
import Prettyprinter

import Language.Elemental.AST.Decl
import Language.Elemental.AST.Expr
import Language.Elemental.AST.Type

-- | Simple unchecked programs in the AST.
type UProgram = UProgramF UDecl

-- | Simple unchecked declarations in the AST.
type UDecl = UDeclF Name ForeignName Address UType UExpr

-- | Simple unchecked expressions in the AST.
type UExpr = Fix (UExprF UType)

-- | Simple unchecked types in the AST.
type UType = Fix UTypeF

-- | The unchecked program functor.
newtype UProgramF decl = UProgram [decl]
    deriving stock (Eq, Ord, Show, Foldable, Functor, Traversable)

-- | The unchecked declaration functor.
data UDeclF name fname addr t expr
    = UBinding name expr
    | UForeignImport name fname t
    | UForeignExport fname expr t
    | UForeignPrimitive name t
    | UForeignAddress name addr t
    deriving stock (Eq, Ord, Show, Foldable, Functor, Traversable)

-- | Independently maps all parameters of a 'UDeclF'.
mapDeclF
    :: (a -> a') -> (b -> b') -> (c -> c') -> (d -> d') -> (e -> e')
    -> UDeclF a b c d e -> UDeclF a' b' c' d' e'
mapDeclF f g h i j = \case
    UBinding a e -> UBinding (f a) (j e)
    UForeignImport a b d -> UForeignImport (f a) (g b) (i d)
    UForeignExport b e d -> UForeignExport (g b) (j e) (i d)
    UForeignPrimitive a d -> UForeignPrimitive (f a) (i d)
    UForeignAddress a c d -> UForeignAddress (f a) (h c) (i d)

-- | The unchecked expression functor.
data UExprF t rec
    = UVar Natural
    | UApp rec rec
    | UTypeApp rec t
    | ULam t rec
    | UTypeLam rec
    | URef Name
    deriving stock (Eq, Ord, Show, Foldable, Functor, Traversable)

instance Bifunctor UExprF where
    bimap f g = \case
        UVar var -> UVar var
        UApp ef ex -> UApp (g ef) (g ex)
        UTypeApp ef tx -> UTypeApp (g ef) (f tx)
        ULam tx ey -> ULam (f tx) (g ey)
        UTypeLam ex -> UTypeLam (g ex)
        URef name -> URef name

instance Bifoldable UExprF where
    bifoldMap f g = \case
        UVar _ -> mempty
        UApp ef ex -> g ef <> g ex
        UTypeApp ef tx -> g ef <> f tx
        ULam tx ey -> f tx <> g ey
        UTypeLam ex -> g ex
        URef _ -> mempty

instance Eq t => Eq1 (UExprF t) where
    liftEq f a b = case (a, b) of
        (UVar aidx, UVar bidx) -> aidx == bidx
        (UApp af ax, UApp bf bx) -> f af bf && f ax bx
        (UTypeApp af ax, UTypeApp bf bx) -> f af bf && ax == bx
        (ULam ax ay, ULam bx by) -> ax == bx && f ay by
        (UTypeLam ax, UTypeLam bx) -> f ax bx
        (URef aname, URef bname) -> aname == bname
        _ -> False

-- | The unchecked type functor.
data UTypeF rec
    = UTypeVar Natural
    | UArrow rec rec
    | UForall rec
    | UIOType rec
    | UPointerType PointerKind rec
    deriving stock (Eq, Ord, Show, Foldable, Functor, Traversable)

instance Eq1 UTypeF where
    liftEq f a b = case (a, b) of
        (UTypeVar aidx, UTypeVar bidx) -> aidx == bidx
        (UArrow ax ay, UArrow bx by) -> f ax bx && f ay by
        (UForall ax, UForall bx) -> f ax bx
        (UIOType ax, UIOType bx) -> f ax bx
        (UPointerType apk ax, UPointerType bpk bx) -> apk == bpk && f ax bx
        _ -> False

-- | A definition name. Used for referencing declarations within expressions.
newtype Name = Name { unName :: ShortText }
    deriving stock (Data)
    deriving newtype (Eq, IsString, Ord, Read, Show)

instance Pretty Name where
    pretty = pretty . toText . unName
