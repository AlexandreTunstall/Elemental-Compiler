{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Primitive functions in the Elemental language.
module Language.Elemental.Primitive
    ( module Language.Elemental.Primitive
    ) where

import Data.Map qualified as M

import Language.Elemental.Syntax.Internal


{-|
    All primitives indexed by name.
    The expression is the corresponding internal expression for the primitive.
-}
primitives :: M.Map Name (Type (), Expr ())
primitives = M.fromList
    [ ("pureIO",
        ( Forall () . Arrow () (TypeVar () 0)
            . SpecialType () . IOType () $ TypeVar () 0
        , InternalExpr () PureIO
        ))
    , ("bindIO",
        ( Forall () . Arrow () (SpecialType () . IOType () $ TypeVar () 0)
            . Forall () . Arrow () (Arrow () (TypeVar () 1)
            . SpecialType () . IOType () $ TypeVar () 0)
            . SpecialType () . IOType () $ TypeVar () 0
        , InternalExpr () BindIO
        ))
    ]
