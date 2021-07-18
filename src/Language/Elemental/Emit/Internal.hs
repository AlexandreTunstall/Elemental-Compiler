{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Internal emitting symbols that other modules need.
module Language.Elemental.Emit.Internal
    ( module Language.Elemental.Emit.Internal
    ) where

import Control.Arrow (Arrow(first))
import Control.Effect.Empty (empty)
import Data.Fix (Fix(Fix))
import Data.Word (Word32)
import LLVM.AST qualified as LLVM
import LLVM.AST.Type qualified as LLVM.Type

import Language.Elemental.Syntax.Internal
import Language.Elemental.Syntax.Synonyms


-- | Marshallable type used to represent a bit.
pattern BitType :: Type
pattern BitType = FA (TV 0 :->: TV 0 :->: TV 0)

-- | Splits a foreign function type into its argument types and its return type.
maybeSplitArrow :: Type -> Maybe ([Type], Type)
maybeSplitArrow ta = case ta of
    tx :->: ty -> first (tx :) <$> maybeSplitArrow ty
    IOT tx -> pure ([], tx)
    _ -> empty

-- | Splits a foreign function type into its argument types and its return type.
maybeSplitArrowA :: AnnType a -> Maybe ([AnnType a], AnnType a)
maybeSplitArrowA (Fix ta) = case extract ta of
    Arrow tx ty -> first (tx :) <$> maybeSplitArrowA ty
    SpecialType (IOType tx) -> pure ([], tx)
    _ -> empty

-- | Converts an Elemental pointer type to an LLVM type.
toMaybePointerType :: Type -> Maybe LLVM.Type
toMaybePointerType t = case t of
    PtrT _ tx -> LLVM.Type.ptr <$> toMaybeArgumentType tx
    _ -> empty

-- | Converts an Elemental type to an LLVM type.
toMaybeArgumentType :: Type -> Maybe LLVM.Type
toMaybeArgumentType t = toMaybeInternalType t >>= \case
    LlvmInt 0 -> empty
    LlvmInt size -> pure $ LLVM.IntegerType size

-- | Converts a type to an internal type.
toMaybeInternalType :: Type -> Maybe (InternalType Type)
toMaybeInternalType t = case t of
    {-
        If you update this function, also update the "unmarshallable type"
        diagnostic so that it remains accurate.
    -}
    BitType -> pure $ LlvmInt 1
    FA (t' :->: TV 0) -> LlvmInt <$> case countTuple t' of
            Just 1 -> empty
            size -> size
    _ -> empty
  where
    countTuple :: Type -> Maybe Word32
    countTuple (BitType :->: t') = succ <$> countTuple t'
    countTuple (TV 0) = pure 0
    countTuple _ = empty
