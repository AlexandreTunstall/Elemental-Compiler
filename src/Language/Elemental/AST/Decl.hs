{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Elemental.AST.Decl
    ( Decl(..)
    , declType
    , DeclScope(..)
    , MaybeCons
    , ForeignName(..)
    , substituteDeclScope
    , substituteDecl
    ) where

import Data.Data (Data)
import Data.Kind qualified as Kind
import Data.String (IsString)
import Data.Text qualified as T
import Data.Text.Short (ShortText, toText)
import Prettyprinter (Pretty(pretty), dquotes)

import Language.Elemental.AST.Expr
import Language.Elemental.AST.Type
import Language.Elemental.Primitive
import Language.Elemental.Singleton


{-|
    Declarations in the AST.

    The first additional parameter indicates the expression scope. The second
    additional parameter indicates an optional expression that the declaration
    adds to the scope if it is present.
-}
type Decl :: [Type] -> Maybe Type -> Kind.Type
data Decl scope mt where
    -- | A binding: @_ = ...@
    Binding :: Expr 'Zero scope t -> Decl scope ('Just t)
    -- | A foreign import: @foreign import _ "foreign_name" : ...@
    ForeignImport
        :: HasForeignType t
        => ForeignName -> SType 'Zero t -> Decl scope ('Just t)
    -- | A foreign export: @foreign export "foreign_name" ... : ...@
    ForeignExport
        :: HasForeignType t
        => ForeignName -> Expr 'Zero scope t -> Decl scope 'Nothing
    -- | A foreign primitive: @foreign primitive _ : ...@
    ForeignPrimitive
        :: CmpNat idx (Length Primitives) ~ 'LT => SNat idx
        -> Decl scope ('Just (Primitives !! idx))
    -- | A foreign address: @foreign address _ 0xDEADBEEF : ...@
    ForeignAddress
        :: (MarshallableType t, IsOpType (Marshall t) ~ 'True)
        => Address -> SPointerKind pk -> SType 'Zero t
        -> Decl scope ('Just ('PointerType pk t))

-- | Gets the type of the expression introduced to the scope by a declaration.
declType
    :: SList (SType 'Zero) scope -> Decl scope mt -> SMaybe (SType 'Zero) mt
declType scope = \case
    Binding expr -> SJust $ exprType SZero scope expr
    ForeignImport _ t -> SJust t
    ForeignExport _ _ -> SNothing
    ForeignPrimitive pidx
        -> SJust $ exprType SZero scope $ primitiveExprs !!^ pidx
    ForeignAddress _ pk t -> SJust $ SPointerType pk t

{-|
    Lists of declarations in the AST.

    The first additional parameter indicates the expression scope. The second
    additional parameter indicates the expressions that the declarations add to
    the scope.
-}
type DeclScope :: [Type] -> [Type] -> Kind.Type
data DeclScope scope rest where
    DeclNil :: DeclScope scope '[]
    DeclCons
        :: Decl scope mt
        -> DeclScope (MaybeCons mt scope) ts
        -> DeclScope scope (t ': ts)

-- | Conses an optional value if it is present, does nothing otherwise.
type MaybeCons :: Maybe a -> [a] -> [a]
type family MaybeCons ma as where
    MaybeCons 'Nothing as = as
    MaybeCons ('Just a) as = a ': as

-- | A name of a foreign symbol. Used for both imported and exported symbols.
newtype ForeignName = ForeignName { unForeignName :: ShortText }
    deriving stock (Data)
    deriving newtype (Eq, IsString, Ord, Read, Show)

instance Pretty ForeignName where
    pretty = dquotes . pretty . T.concatMap escape . toText . unForeignName
      where
        escape :: Char -> T.Text
        escape '\\' = T.pack "\\\\"
        escape '\"' = T.pack "\\\""
        escape c = T.pack [c]

-- | Substitutes an expression at an index of a declaration list's scope.
substituteDeclScope
    :: CmpNat idx (Length scope) ~ 'LT => SList (SType 'Zero) scope -> SNat idx
    -> Expr 'Zero (Remove idx scope) (scope !! idx)
    -> DeclScope scope rest -> DeclScope (Remove idx scope) rest
substituteDeclScope scope idx sub = \case
    DeclNil -> DeclNil
    DeclCons decl decls -> DeclCons (substituteDecl scope idx sub decl)
        $ case declType scope decl of
            SNothing -> substituteDeclScope scope idx sub decls
            SJust t -> withProof (insZeroP t $ sRemove idx scope)
                $ substituteDeclScope (t :^ scope) (SSucc idx)
                    (incrementExpr SZero (sRemove idx scope) SZero t sub) decls

-- | Substitutes an expression at an index of a declaration's scope.
substituteDecl
    :: CmpNat idx (Length scope) ~ 'LT => SList (SType 'Zero) scope -> SNat idx
    -> Expr 'Zero (Remove idx scope) (scope !! idx)
    -> Decl scope mt -> Decl (Remove idx scope) mt
substituteDecl scope idx sub = \case
    Binding expr -> Binding $ substituteExpr SZero scope idx sub expr
    ForeignImport fname t -> ForeignImport fname t
    ForeignExport fname expr
        -> ForeignExport fname $ substituteExpr SZero scope idx sub expr
    ForeignPrimitive pfin -> ForeignPrimitive pfin
    ForeignAddress addr pk t -> ForeignAddress addr pk t
