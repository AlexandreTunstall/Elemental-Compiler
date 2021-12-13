{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Language.Elemental.Pretty
    ( prettyDecl
    , prettyExpr
    , prettyType
    , prettyLlvmType
    , prettyNat
    -- * Unchecked
    , prettyUProgramF
    , prettyUDeclF
    , prettyUDeclFAbbrev
    , prettyUExprF
    , prettyUTypeF
    -- ** Pure
    , prettyUProgram
    , prettyUDecl
    , prettyUDeclAbbrev
    , prettyUExpr
    , prettyUType
    -- ** Annotated
    , prettyPProgram
    , prettyPDecl
    , prettyPDeclAbbrev
    , prettyPExpr
    , prettyPType
    , prettyAnn
    -- * Miscellaneous
    , withPrec
    ) where

import Data.Bifunctor (first)
import Data.Fix (foldFix)
import Prettyprinter

import Language.Elemental.Algebra
import Language.Elemental.AST.Decl
import Language.Elemental.AST.Expr
import Language.Elemental.AST.Type
import Language.Elemental.AST.Unchecked
import Language.Elemental.Parser
import Language.Elemental.Primitive
import Language.Elemental.Singleton


-- | Prettyprints a declaration.
prettyDecl :: Decl '[] mt -> Doc ann
prettyDecl = \case
    Binding expr -> "_ =" <+> prettyExpr 0 expr
    ForeignImport fname t -> "foreign import _" <+> pretty fname
        <+> ":" <+> prettyType 0 t
    ForeignExport fname expr -> "foreign export" <+> pretty fname
        <+> prettyExpr 2 expr
        <+> ":" <+> prettyType 0 (exprType SZero SNil expr)
    ForeignPrimitive idx -> "foreign primitive"
        <+> pretty (Name $ getPrimitiveName idx)
        <+> ":" <+> prettyType 0 (exprType SZero SNil $ primitiveExprs !!^ idx)
    ForeignAddress addr pk tx -> "foreign address _" <+> pretty addr
        <+> ":" <+> prettyType 0 (SPointerType pk tx)

-- | Prettyprints an expression with the given precedence.
prettyExpr :: Int -> Expr tscope scope t -> Doc ann
prettyExpr = flip $ \case
    Var vidx -> withPrec 2 $ prettyNat vidx
    App ef ex -> withPrec 1 $ prettyExpr 1 ef <+> prettyExpr 2 ex
    TypeApp ef tx -> withPrec 1 $ prettyExpr 1 ef <+> "@" <> prettyType 2 tx
    Lam tx ey -> withPrec 0 $ "λ" <> prettyType 2 tx <+> prettyExpr 0 ey
    TypeLam ex -> withPrec 0 $ "Λ" <+> prettyExpr 0 ex
    Addr addr _ _ -> withPrec 3 $ braces $ "addr" <+> pretty addr
    LlvmOperand lt _ -> withPrec 3 $ braces $ "op" <+> prettyLlvmType lt
    LlvmIO lt _ -> withPrec 3 $ braces $ "io op" <+> prettyLlvmType lt
    PureIO -> withPrec 3 $ braces "pureIO"
    BindIO -> withPrec 3 $ braces "bindIO"
    LoadPointer -> withPrec 3 $ braces "loadPointer"
    StorePointer -> withPrec 3 $ braces "storePointer"
    Call _ _ ltargs ltret -> withPrec 3 $ braces $ "call"
        <+> prettyLlvmType ltret <> parens (concatWith (surround ", ")
            $ demoteList prettyLlvmType ltargs)
    IsolateBit bidx size -> withPrec 3 $ braces $ "isolate"
        <+> prettyNat bidx <+> prettyLlvmType (SLlvmInt size)
    InsertBit size -> withPrec 3 $ braces
        $ "insert" <+> prettyLlvmType (SLlvmInt size)
    TestBit ex -> withPrec 3 $ braces $ "testBit" <+> prettyExpr 0 ex

-- | Prettyprints a type with the given precedence.
prettyType :: Int -> SType tscope t -> Doc ann
prettyType = flip $ \case
    STypeVar tvidx -> withPrec 2 $ prettyNat tvidx
    SArrow tx ty -> withPrec 0 $ prettyType 1 tx <+> "→" <+> prettyType 0 ty
    SForall tx -> withPrec 0 $ "∀" <+> prettyType 0 tx
    SIOType tx -> withPrec 1 $ "IO" <+> prettyType 2 tx
    SPointerType pk tx -> withPrec 1 $ case pk of
        SReadPointer -> "ReadPointer" <+> prettyType 2 tx
        SWritePointer -> "WritePointer" <+> prettyType 2 tx
    SLlvmType lt -> withPrec 3 $ prettyLlvmType lt

-- | Prettyprints an t'LlvmType'.
prettyLlvmType :: SLlvmType lt -> Doc ann
prettyLlvmType = \case
    SLlvmInt size -> "i" <> prettyNat size

-- | Prettyprints a 'Nat'.
prettyNat :: SNat nat -> Doc ann
prettyNat = pretty . toNatural

-- | Prettyprints a simple unchecked program.
prettyUProgram :: UProgram -> Doc ann
prettyUProgram = prettyUProgramF . fmap prettyUDecl

-- | Prettyprints a parsed unchecked program.
prettyPProgram :: PProgram -> Doc ann
prettyPProgram = prettyUProgramF . fmap prettyPDecl . sndP

-- | Prettyprints an unchecked program.
prettyUProgramF :: UProgramF (Doc ann) -> Doc ann
prettyUProgramF (UProgram decls) = concatWith declSep decls
  where
    declSep :: Doc ann -> Doc ann -> Doc ann
    declSep x y = x <> flatAlt hardline "; " <> y

-- | Prettyprints a simple unchecked declaration.
prettyUDecl :: UDecl -> Doc ann
prettyUDecl = prettyUDeclF
    . mapDeclF pretty pretty pretty (flip prettyUType) (flip prettyUExpr)

-- | Prettyprints a parsed unchecked declaration.
prettyPDecl :: PDecl -> Doc ann
prettyPDecl = prettyUDeclF . mapDeclF prettyAnn prettyAnn prettyAnn
    (flip prettyPType) (flip prettyPExpr) . sndP

-- | Prettyprints an unchecked declaration.
prettyUDeclF
    :: UDeclF (Doc ann) (Doc ann) (Doc ann) (Int -> Doc ann) (Int -> Doc ann)
    -> Doc ann
prettyUDeclF = \case
    UBinding name expr -> name <+> "=" <+> expr 0
    UForeignImport name fname t
        -> "foreign import" <+> name <+> fname <+> ":" <+> t 0
    UForeignExport fname expr t
        -> "foreign export" <+> fname <+> expr 2 <+> ":" <+> t 0
    UForeignPrimitive name t -> "foreign primitive" <+> name <+> ":" <+> t 0
    UForeignAddress name addr t
        -> "foreign address" <+> name <+> addr <+> ":" <+> t 0

-- | Prettyprints an abbreviated version of a simple unchecked declaration.
prettyUDeclAbbrev :: UDecl -> Doc ann
prettyUDeclAbbrev = prettyUDeclFAbbrev . mapDeclF pretty pretty pretty id id

-- | Prettyprints an abbreviated version of a parsed unchecked declaration.
prettyPDeclAbbrev :: PDecl -> Doc ann
prettyPDeclAbbrev
    = prettyUDeclFAbbrev . mapDeclF prettyAnn prettyAnn prettyAnn id id . sndP

-- | Prettyprints an abbreviated version of an unchecked declaration.
prettyUDeclFAbbrev :: UDeclF (Doc ann) (Doc ann) (Doc ann) t expr -> Doc ann
prettyUDeclFAbbrev = \case
    UBinding name _ -> name <+> "=" <+> "[...]"
    UForeignImport name fname _ -> "foreign import" <+> name <+> fname
    UForeignExport fname _ _ -> "foreign export" <+> fname
    UForeignPrimitive name _ -> "foreign primitive" <+> name
    UForeignAddress name addr _ -> "foreign address" <+> name <+> addr

-- | Prettyprints a simple unchecked expression with the given precedence.
prettyUExpr :: Int -> UExpr -> Doc ann
prettyUExpr = flip . foldFix $ prettyUExprF . first (flip prettyUType)

-- | Prettyprints a parsed unchecked expression with the given precedence.
prettyPExpr :: Int -> PExpr -> Doc ann
prettyPExpr = flip . foldFix $ prettyUExprF . first (flip prettyPType) . sndP1

-- | Prettyprints an unchecked expression with the given precedence.
prettyUExprF :: UExprF (Int -> Doc ann) (Int -> Doc ann) -> Int -> Doc ann
prettyUExprF = \case
    UVar var -> withPrec 2 $ pretty var
    UApp ef ex -> withPrec 1 $ ef 1 <+> ex 2
    UTypeApp ef tx -> withPrec 1 $ ef 1 <+> "@" <> tx 2
    ULam tx ey -> withPrec 0 $ "λ" <> tx 2 <+> ey 0
    UTypeLam ex -> withPrec 0 $ "Λ" <+> ex 0
    URef name -> withPrec 2 $ pretty name

-- | Prettyprints a simple unchecked type with the given precedence.
prettyUType :: Int -> UType -> Doc ann
prettyUType = flip $ foldFix prettyUTypeF

-- | Prettyprints a parsed unchecked type with the given precedence.
prettyPType :: Int -> PType -> Doc ann
prettyPType = flip . foldFix $ prettyUTypeF . sndP1

-- | Prettyprints an unchecked type with the given precedence.
prettyUTypeF :: UTypeF (Int -> Doc ann) -> Int -> Doc ann
prettyUTypeF = \case
    UTypeVar tvar -> withPrec 2 $ pretty tvar
    UArrow tx ty -> withPrec 0 $ tx 1 <+> "→" <+> ty 0
    UForall tx -> withPrec 0 $ "∀" <+> tx 0
    UIOType tx -> withPrec 1 $ "IO" <+> tx 2
    UPointerType pk tx -> withPrec 1 $ case pk of
        ReadPointer -> "ReadPointer" <+> tx 2
        WritePointer -> "WritePointer" <+> tx 2

-- | Prettyprints an annotated type by discarding the annotation.
prettyAnn :: Pretty b => a * b -> Doc ann
prettyAnn = pretty . sndP

{-|
    Wraps the document in parenthesis if the current precedence is greater than
    the document's precedence.
-}
withPrec
    :: Int {-^ The document's precedence -} -> Doc ann
    -> Int {-^ The current precedence -} -> Doc ann
withPrec req r act
    | act > req = parens r
    | otherwise = r
