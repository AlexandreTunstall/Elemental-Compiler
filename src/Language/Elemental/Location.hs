{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definitions for tracking the source location of diagnostics.
module Language.Elemental.Location
    ( IsLoc(..)
    , SourceLocation(..)
    , SrcSpan(..)
    ) where

import Data.Data (Data)
import Data.Fix (Fix(Fix))
import Prettyprinter (Doc, Pretty(pretty), line, nest, parens, vsep)
import Text.Megaparsec.Pos (Pos, SourcePos(SourcePos), unPos)

import Language.Elemental.Algebra


-- | A source span consisting of a start position and an end position.
data SrcSpan = SrcSpan
    SourcePos
    -- ^ The start position.
    SourcePos
    -- ^ The end position.
    deriving stock (Data, Show)

instance Pretty SrcSpan where
    pretty (SrcSpan (SourcePos fb lb cb) (SourcePos fe le ce)) = if fb == fe
        then pretty fb <> ":" <> if lb == le
            then prettyPos lb <> ":" <> if cb == ce
                then prettyPos cb
                else prettyPos cb <> "-" <> prettyPos ce
            else parens (prettyPos lb <> "," <> prettyPos cb)
                <> "-" <> parens (prettyPos le <> "," <> prettyPos ce)
        else parens (pretty fb <> "," <> prettyPos lb <> "," <> prettyPos cb)
            <> "-" <> parens (pretty fe <> "," <> prettyPos le <> ","
                <> prettyPos ce)

-- | A source location used when reporting diagnostics.
data SourceLocation
    -- | A source span as a source location.
    = SourceSpan SrcSpan
    {-|
        Multiple source locations. Useful for errors that originate from
        multiple locations such as duplicate declarations with the same name.
    -}
    | SourceMultiple [SourceLocation]
    {-|
        An unknown source location.

        This should be avoided unless if the source location is genuinely
        unknown, otherwise diagnostics will be severely less helpful.
    -}
    | SourceUnknown
    deriving stock (Show)

instance Pretty SourceLocation where
    pretty loc = case loc of
        SourceSpan srcSpan -> pretty srcSpan
        SourceMultiple locs -> "multiple locations:"
            <> nest 4 (line <> vsep (pretty <$> locs))
        SourceUnknown -> "<unknown>"

prettyPos :: Pos -> Doc ann
prettyPos = pretty . unPos

{-|
    Class for data structures that represent a source location.

    Functions that emit diagnostics should accept any instance of this class
    instead of requiring a specific representation for AST labels.
    Doing so enables labelling the AST with customisable metadata.
-}
class IsLoc l where
    -- | Gets the source location.
    getLoc :: l -> SourceLocation

instance IsLoc SrcSpan where
    getLoc = SourceSpan

instance IsLoc SourceLocation where
    getLoc = id

instance IsLoc l => IsLoc [l] where
    getLoc = SourceMultiple . fmap getLoc

instance IsLoc (SrcSpan * a) where
    getLoc (P loc _) = getLoc loc

instance IsLoc ((K SrcSpan * f) a) where
    getLoc (P1 (K1 loc) _) = getLoc loc

instance IsLoc (f (Fix f)) => IsLoc (Fix f) where
    getLoc (Fix f) = getLoc f
