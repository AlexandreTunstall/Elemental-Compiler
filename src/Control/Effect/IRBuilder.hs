{-|
    Provides the 'IRBuilder' effect, which simplifies the generation of LLVM
    blocks and is analogous to 'LLVM.IRBuilder.MonadIRBuilder'.

    Most of the operations in this module are adapted from "LLVM.IRBuilder" to
    work with fused-effects.
-}
module Control.Effect.IRBuilder
    ( IRBuilder
    -- * High-level
    , emitInstr
    , emitInstrVoid
    , emitTerm
    , block
    , emitBlockStart
    , fresh
    , currentBlock
    -- * Low-level
    , ensureBlock
    , modifyBlock
    , freshName
    , freshUnName
    -- * Re-exports
    , Algebra
    , Has
    , run
    ) where

import Control.Effect.State (Algebra, Has, State, gets, modify, run)
import Data.ByteString.Short (ShortByteString)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))
import LLVM.AST
    ( BasicBlock(BasicBlock)
    , Instruction
    , Name(Name, UnName)
    , Named(Do, (:=))
    , Operand(LocalReference)
    , Terminator(Ret)
    , Type
    )
import LLVM.IRBuilder (IRBuilderState(..), PartialBlock(..), emptyPartialBlock)
import LLVM.IRBuilder.Internal.SnocList (getSnocList, snoc)


-- | The IRBuilder effect.
type IRBuilder = State IRBuilderState

-- | Finishes the previous block and starts a new block with a fresh label.
block :: Has IRBuilder sig m => m Name
block = do
    nm <- fresh
    nm <$ emitBlockStart nm

-- | Emits an instruction.
emitInstr :: Has IRBuilder sig m => Type -> Instruction -> m Operand
emitInstr retTy instr = do
    ensureBlock
    nm <- fresh
    modifyBlock $ \bb -> bb
        { partialBlockInstrs = partialBlockInstrs bb `snoc` (nm := instr) }
    pure $ LocalReference retTy nm

-- | Emits an instruction that returns @void@.
emitInstrVoid :: Has IRBuilder sig m => Instruction -> m ()
emitInstrVoid instr = modifyBlock $ \bb -> bb
    { partialBlockInstrs = partialBlockInstrs bb `snoc` Do instr }

-- | Emits a basic block terminator.
emitTerm :: Has IRBuilder sig m => Terminator -> m ()
emitTerm term = modifyBlock $ \bb -> bb { partialBlockTerm = Just $ Do term }

{-|
    Creates a new partial block with a fresh label if one doesn't already exist.
    This is useful for controlling the order in which labels are assigned.
-}
ensureBlock :: Has IRBuilder sig m => m ()
ensureBlock = do
    mbb <- gets builderBlock
    case mbb of
        Nothing -> do
            nm <- freshUnName
            modify $ \s -> s { builderBlock = Just $! emptyPartialBlock nm }
        Just _ -> pure ()

-- | Modifies the current partial block.
modifyBlock :: Has IRBuilder sig m => (PartialBlock -> PartialBlock) -> m ()
modifyBlock f = do
    mbb <- gets builderBlock
    case mbb of
        Nothing -> do
            nm <- freshUnName
            modify $ \s -> s { builderBlock = Just $! f $ emptyPartialBlock nm }
        Just bb -> modify $ \s -> s { builderBlock = Just $! f bb }

-- | Gets the name of the current block, creating a block if there isn't any.
currentBlock :: Has IRBuilder sig m => m Name
currentBlock = gets (fmap partialBlockName . builderBlock) >>= maybe block pure

-- | Generates a fresh name.
fresh :: Has IRBuilder sig m => m Name
fresh = gets builderNameSuggestion >>= maybe freshUnName freshName

-- | Generates a fresh name from the suggested bytestring.
freshName :: Has IRBuilder sig m => ShortByteString -> m Name
freshName suggestion = do
    usedNames <- gets builderUsedNames
    let nameCount = fromMaybe 0 $ M.lookup suggestion usedNames
        unusedName = suggestion <> fromString ("_" <> show nameCount)
        updatedUsedNames = M.insert suggestion (nameCount + 1) usedNames
    modify $ \s -> s { builderUsedNames = updatedUsedNames }
    pure $ Name unusedName

-- | Generates a fresh unname.
freshUnName :: Has IRBuilder sig m => m Name
freshUnName = do
    n <- gets builderSupply
    modify $ \s -> s { builderSupply = 1 + n }
    pure $ UnName n

-- | Finishes the previous block and starts a new block with the given label.
emitBlockStart :: Has IRBuilder sig m => Name -> m ()
emitBlockStart nm = do
    mbb <- gets builderBlock
    case mbb of
        Nothing -> pure ()
        Just bb -> do
            let instrs = getSnocList $ partialBlockInstrs bb
                newBb = BasicBlock (partialBlockName bb) instrs
                    . fromMaybe (Do $ Ret Nothing []) $ partialBlockTerm bb
            modify $ \s -> s { builderBlocks = builderBlocks s `snoc` newBb }
    modify $ \s -> s { builderBlock = Just $ emptyPartialBlock nm }
