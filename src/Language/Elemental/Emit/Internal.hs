{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Internal emitting symbols that other modules need.
module Language.Elemental.Emit.Internal
    ( module Language.Elemental.Emit.Internal
    ) where

import Control.Arrow (Arrow(first))
import Control.Effect.Empty (empty)
import Data.Word (Word32)
import LLVM.AST qualified as LLVM

import Language.Elemental.Syntax.Internal hiding ((:$), (:@), (:\), (:->))


-- | Marshallable type used to represent a bit.
pattern BitType :: a -> Type a
pattern BitType l <- Forall l (Arrow _ (TypeVar _ 0)
    (Arrow _ (TypeVar _ 0) (TypeVar _ 0)))
  where
    BitType l = Forall l . Arrow l (TypeVar l 0)
        $ Arrow l (TypeVar l 0) (TypeVar l 0)

-- | Splits a foreign function type into its argument types and its return type.
maybeSplitArrow :: Type a -> Maybe ([Type a], Type a)
maybeSplitArrow ta = case ta of
    Arrow _ tx ty -> first (tx :) <$> maybeSplitArrow ty
    SpecialType _ (IOType _ tx) -> pure ([], tx)
    _ -> empty

-- | Converts an Elemental type to an LLVM type.
toMaybeArgumentType :: Type a -> Maybe LLVM.Type
toMaybeArgumentType t = toMaybeInternalType t >>= \case
    LlvmInt 0 -> empty
    LlvmInt size -> pure $ LLVM.IntegerType size

-- | Converts a type to an internal type.
toMaybeInternalType :: Type a -> Maybe (InternalType a)
toMaybeInternalType t = case t of
    {-
        If you update this function, also update the "unmarshallable type"
        diagnostic so that it remains accurate.
    -}
    BitType _ -> pure $ LlvmInt 1
    Forall _ (Arrow _ t' (TypeVar _ 0)) -> LlvmInt <$> case countTuple t' of
        Just 1 -> empty
        size -> size
    _ -> empty
  where
    countTuple :: Type a -> Maybe Word32
    countTuple (Arrow _ (BitType _) t') = succ <$> countTuple t'
    countTuple (TypeVar _ 0) = pure 0
    countTuple _ = empty
