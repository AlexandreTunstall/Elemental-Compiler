{-# LANGUAGE ImportQualifiedPost #-}

-- | The Elemental language compiler library.
module Language.Elemental
    ( -- * Overview
    -- $overview
      version
    
    -- * AST
    , module Language.Elemental.AST.Decl
    , module Language.Elemental.AST.Expr
    , module Language.Elemental.AST.Program
    , module Language.Elemental.AST.Type
    , module Language.Elemental.AST.Unchecked
    , module Language.Elemental.Primitive
    , module Language.Elemental.Algebra
    , module Language.Elemental.Singleton

    -- * Parsing
    -- $parsing
    , module Language.Elemental.Location
    , module Language.Elemental.Parser

    -- * Prettyprinting
    -- $prettyprinting
    , module Language.Elemental.Pretty

    -- * Validating
    -- $validating
    , module Language.Elemental.Diagnostic
    , module Language.Elemental.TypeCheck

    -- * Emitting
    -- $emitting
    , module Language.Elemental.Emit
    ) where

import Data.Version (Version)

import Language.Elemental.Algebra
import Language.Elemental.AST.Decl
import Language.Elemental.AST.Expr
import Language.Elemental.AST.Program
import Language.Elemental.AST.Type
import Language.Elemental.AST.Unchecked
import Language.Elemental.Diagnostic
import Language.Elemental.Emit
import Language.Elemental.Location
import Language.Elemental.Parser
import Language.Elemental.Pretty
import Language.Elemental.Primitive
import Language.Elemental.Singleton
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
    (3) Emitting the AST into the corresponding LLVM.
-}

{- $ast
    The abstract syntax tree (AST) is how Elemental programs are manipulated
    throughout the compiler. It can either be parsed from Elemental source code
    or generated directly.

    Note that internal constructors are exposed and can be safely used, however
    users should avoid generating them directly where possible. To this end,
    the "Language.Elemental.Primitive" module is most useful.
-}

{- $parsing
    The "Language.Elemental.Parser" module exports parsers for various parts of
    the language.

    To label the AST with helpful metadata, the exported parsers use a special
    parser monad that can be converted into a MegaParsec parser using
    'mkParser'.
-}

{- $prettyprinting
    The AST can be prettyprinted for storage and debugging purposes.

    In the context of parsed ASTs, the parsers are right inverses of the
    corresponding prettyprinters, meaning that the prettyprinter output can be
    reparsed to get the original AST back. Functions that only reannotate the
    AST preserve this property.

    ASTs obtained through methods other than parsing might not satisfy this
    property due to the use of illegal terms or internal terms. Nonetheless, the
    prettyprinters will still be able to generate helpful output for debugging.
-}

{- $validating
    There are two versions of the AST:

    * The validated AST, which only admits valid programs and thus can be hard
      to generate.
    * The unchecked AST, which is easier to generate, but must be validated.

    The "Language.Elemental.TypeCheck" module exports functions for validating
    the unchecked AST.

    The AST generated by validating satisfies the right inverse property of the
    prettyprinter even if the unchecked AST that was validated was not parsed.
    In other words, users that need to generate an unchecked AST and store it
    for later use can safely store the validated AST.
-}

{- $emitting
    Programs can be emitted as LLVM using 'emitProgram'.

    The compiler also exposes its other emitting functions, however their
    interface may be more volatile as there's no clear use case for them.
-}
