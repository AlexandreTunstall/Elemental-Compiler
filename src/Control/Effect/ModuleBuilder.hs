{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-|
    Provides the 'ModuleBuilder' effect, which simplifies the generation of LLVM
    modules and is analogous to 'LLVM.IRBuilder.MonadModuleBuilder'.

    Most of the operations in this module are adapted from "LLVM.IRBuilder" to
    work with fused-effects.
-}
module Control.Effect.ModuleBuilder
    ( ModuleBuilder
    -- * High-level
    , function
    , extern
    -- * Low-level
    , emitDefn
    -- * Re-exports
    , Algebra
    , Has
    , run
    ) where

import Control.Effect.State (State, modify)
import LLVM.AST
    ( Definition(GlobalDefinition)
    , Name
    , Operand(ConstantOperand, LocalReference)
    , Parameter(Parameter)
    , Type(FunctionType)
    , functionDefaults
    , mkName
    )
import LLVM.AST.Constant (Constant(GlobalReference))
import LLVM.AST.Global
    (Global(basicBlocks, linkage, name, parameters, returnType))
import LLVM.AST.Linkage (Linkage(External))
import LLVM.AST.Type (ptr)
import LLVM.IRBuilder (ModuleBuilderState(..))
import LLVM.IRBuilder.Internal.SnocList (snoc)

import Control.Carrier.IRBuilder


-- | The ModuleBuilder effect.
type ModuleBuilder = State ModuleBuilderState

-- | Emits a definition.
emitDefn :: Has ModuleBuilder sig m => Definition -> m ()
emitDefn def = modify $ \s -> s { builderDefs = builderDefs s `snoc` def }

-- | Emits a function definition.
function
    :: Has ModuleBuilder sig m
    => Name
    -- ^ The name of the function.
    -> [Type]
    -- ^ The types of the function arguments.
    -> Type
    -- ^ The return type of the function.
    -> Linkage
    -- ^ The linkage of the function.
    -> ([Operand] -> IRBuilderC m ())
    -- ^ A function that builds the function's basic blocks from its arguments.
    -> m Operand
function nm argTys retTy link body = do
    (blocks, paramNames) <- runIRBuilder emptyIRBuilder $ do
        paramNames <- traverse (const fresh) argTys
        body $ zipWith LocalReference argTys paramNames
        pure paramNames
    let def = GlobalDefinition functionDefaults
            { name = nm
            , parameters
                = (($ []) <$> zipWith Parameter argTys paramNames, False)
            , returnType = retTy
            , basicBlocks = blocks
            , linkage = link
            }
        funTy = ptr $ FunctionType retTy argTys False
    ConstantOperand (GlobalReference funTy nm) <$ emitDefn def

-- | Emits an external function declaration.
extern
    :: Has ModuleBuilder sig m
    => Name
    -- ^ The name of the function.
    -> [Type]
    -- ^ The types of the function arguments.
    -> Type
    -- ^ The return type of the function.
    -> m Operand
extern nm argTys retTy = do
    emitDefn $ GlobalDefinition functionDefaults
        { name = nm
        , linkage = External
        , parameters = (($ []) . ($ mkName "") . Parameter <$> argTys, False)
        , returnType = retTy
        }
    let funTy = ptr $ FunctionType retTy argTys False
    pure . ConstantOperand $ GlobalReference funTy nm
