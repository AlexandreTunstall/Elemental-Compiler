{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- "Redundant" constraints are needed to prove the completeness of patterns.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Primitive functions in the Elemental language.
module Language.Elemental.Primitive
    ( module Language.Elemental.Primitive
    ) where

import Data.Text.Short (ShortText)

import Language.Elemental.AST.Expr
import Language.Elemental.AST.Type
import Language.Elemental.Singleton


-- | The types of all primitive expressions.
type Primitives :: [Type]
type Primitives =
    '[ PureIOType
    , BindIOType
    , LoadPointerType
    , StorePointerType
    ]

{-|
    All primitive expressions.

    The expression value may be internal.
-}
primitiveExprs :: SList (Expr 'Zero scope) Primitives
primitiveExprs = PureIO :^ BindIO :^ LoadPointer :^ StorePointer :^ SNil

{-|
    Gets the index of a primitive by its name. See the source code for which
    primitive names are supported.

    Using this may be sounder than directly indexing 'Primitives' because the
    order of the primitives may change between versions.
-}
getPrimitive
    :: ShortText
    -> r -> (forall idx. CmpNat idx (Length Primitives) ~ 'LT => SNat idx -> r)
    -> r
getPrimitive pname abort cont = case pname of
    "pureIO" -> cont SZero
    "bindIO" -> cont $ SSucc SZero
    "loadPointer" -> cont $ SSucc $ SSucc SZero
    "storePointer" -> cont $ SSucc $ SSucc $ SSucc SZero
    _ -> abort

{-|
    Gets the name of the primitive at the given index. Useful for showing a
    user-friendly name when only the index is known.
-}
getPrimitiveName
    :: CmpNat idx (Length Primitives) ~ 'LT => SNat idx -> ShortText
getPrimitiveName = \case
    SZero -> "pureIO"
    SSucc SZero -> "bindIO"
    SSucc (SSucc SZero) -> "loadPointer"
    SSucc (SSucc (SSucc SZero)) -> "storePointer"
    SSucc (SSucc (SSucc (SSucc idx))) -> case idx of {}
