{-|
    A carrier for 'IRBuilder' effects.
-}
module Control.Carrier.IRBuilder
    ( IRBuilderC
    , runIRBuilder
    , IRBuilderState
    , emptyIRBuilder
    , module Control.Effect.IRBuilder
    ) where

import Control.Arrow (Arrow(first))
import Control.Carrier.State.Church (StateC, runState)
import LLVM.AST (BasicBlock)
import LLVM.IRBuilder.Internal.SnocList (getSnocList)
import LLVM.IRBuilder.Monad (IRBuilderState(builderBlocks), emptyIRBuilder)

import Control.Effect.IRBuilder


-- | A carrier for 'IRBuilder' effects.
type IRBuilderC = StateC IRBuilderState

{-|
    Runs an 'IRBuilderC' with the given initial state, outputting a list of
    basic blocks.
-}
runIRBuilder
    :: Applicative m => IRBuilderState -> IRBuilderC m a -> m ([BasicBlock], a)
runIRBuilder = runState . curry $ pure . first (getSnocList . builderBlocks)
