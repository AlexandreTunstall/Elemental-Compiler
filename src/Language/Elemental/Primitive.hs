{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Primitive functions in the Elemental language.
module Language.Elemental.Primitive
    ( module Language.Elemental.Primitive
    ) where

import Data.Map qualified as M

import Language.Elemental.Syntax.Internal
import Language.Elemental.Syntax.Synonyms


{-|
    All primitives indexed by name.
    The expression is the corresponding internal expression for the primitive.
-}
primitives :: M.Map Name (Type, Expr)
primitives = M.fromList
    [ ("pureIO",
        ( FA $ TV 0 :->: IOT (TV 0)
        , IE PureIO
        ))
    , ("bindIO",
        ( FA $ IOT (TV 0) :->: FA ((TV 1 :->: IOT (TV 0)) :->: IOT (TV 0))
        , IE BindIO
        ))
    , ("loadPointer",
        ( FA $ PtrT ReadPointer (TV 0) :->: IOT (TV 0)
        , IE LoadPointer
        ))
    , ("storePointer",
        ( FA $ PtrT WritePointer (TV 0) :->: TV 0 :->: IOT (FA $ TV 0 :->: TV 0)
        , IE StorePointer
        ))
    ]
