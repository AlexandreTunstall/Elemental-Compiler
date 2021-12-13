{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Elemental.AST.Program
    ( Program(..)
    ) where

import Language.Elemental.AST.Decl


-- | Programs in the AST.
data Program where
    Program :: DeclScope '[] scope -> Program
