{-# LANGUAGE ImportQualifiedPost #-}

-- | The Elemental language compiler library.
module Language.Elemental
    ( -- * Overview
    -- $overview
      version

    -- * Parsing
    -- $parsing
    , module Language.Elemental.Location
    , module Language.Elemental.Parser

    -- * Validating
    -- $validating
    , module Language.Elemental.Diagnostic
    , module Language.Elemental.Syntax
    , module Language.Elemental.TypeCheck

    -- * Interpreting
    -- $interpreting
    , module Language.Elemental.Normalise
    , module Language.Elemental.Rewrite

    -- * Emitting
    -- $emitting
    , module Language.Elemental.Emit
    ) where

import Data.Version (Version)

import Language.Elemental.Diagnostic
import Language.Elemental.Emit
import Language.Elemental.Location
import Language.Elemental.Normalise
import Language.Elemental.Parser
import Language.Elemental.Rewrite
import Language.Elemental.Syntax
import Language.Elemental.TypeCheck
import Paths_elemental qualified as Paths


-- | The current version of the Elemental compiler library.
version :: Version
version = Paths.version

{- $overview
    Compilation of Elemental is split into three steps:

    (1) Generating the AST, either by parsing text or by generating it in a
        frontend compiler.
    (2) Validating the AST: ensuring that declarations and types are correct.
    (3) Interpreting the AST directly or emitting corresponding LLVM.
-}

{- $parsing
    The "Language.Elemental.Parser" module exports parsers for various parts of
    the language.

    To label the AST with helpful metadata, the exported parsers use a special
    parser monad that can be converted into a MegaParsec parser using
    'mkParser'.
-}

{- $validating
    The AST is automatically validated when using 'mkProgram' to create a
    t'Program' from a list of declarations.

    The AST may be type checked manually using 'tcDecl'. This is useful for
    verifying that interpretation and normalisation preserve type correctness,
    but there is otherwise little reason to do so.
-}

{- $interpreting
    Programs can be interpreted using normalisation functions such as
    'normaliseProgram'.
    Functions that rewrite expressions require a 'Rewriter' effect which enables
    the sequence of rewrite steps to be tracked and can be useful for debugging
    expressions or the rewrite rules themselves.

    To interpret programs with foreign calls, it may be easier to emit LLVM and
    then use the LLVM interpreter.
-}

{- $emitting
    Programs can be emitted as LLVM using 'emitProgram'.
    This function rewrites expressions under the hood, so a 'Rewriter' effect
    must be present and can be used to trace the rewrite steps.
-}
