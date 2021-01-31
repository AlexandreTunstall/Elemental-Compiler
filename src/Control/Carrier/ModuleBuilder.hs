-- | A carrier for 'ModuleBuilder' effects.
module Control.Carrier.ModuleBuilder
    ( ModuleBuilderC
    , runModuleBuilder
    , ModuleBuilderState
    , emptyModuleBuilder
    , module Control.Effect.ModuleBuilder
    ) where

import Control.Carrier.State.Church (StateC, runState)
import LLVM.AST (Definition)
import LLVM.IRBuilder.Internal.SnocList (getSnocList)
import LLVM.IRBuilder.Module (ModuleBuilderState(..), emptyModuleBuilder)

import Control.Effect.ModuleBuilder


-- | A carrier for 'ModuleBuilder' effects.
type ModuleBuilderC = StateC ModuleBuilderState

{-|
    Runs a 'ModuleBuilder" with the given initial state and passes the resulting
    list of definitions to the given continuation.
-}
runModuleBuilder
    :: ([Definition] -> a -> m b) -> ModuleBuilderState -> ModuleBuilderC m a
    -> m b
runModuleBuilder cont = runState $ cont . getSnocList . builderDefs
