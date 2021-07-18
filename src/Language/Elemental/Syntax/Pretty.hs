{-
    We disable the orphan instances warning.
    This is a sane thing to do as long as:

     * All modules that re-export "Language.Elemental.Syntax.Internal" also
       import this module.
     * This module only defines orphan instances for types defined in
       "Language.Elemental.Syntax.Internal".
    
    In the future, the basic 'Pretty' instances might be replaced with functions
    that also annotate the document, in which case the warning can be reenabled.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Prettyprinting for the Elemental AST.
module Language.Elemental.Syntax.Pretty
    ( prettyDeclHead
    , prettyAnnExpr0
    , prettyAnnExpr1
    , prettyAnnExpr2
    , prettyExpr0
    , prettyExpr1
    , prettyExpr2
    , prettyExprF
    , prettyAnnType0
    , prettyAnnType1
    , prettyType0
    , prettyType1
    , prettyTypeF
    , prettyAnnType
    , prettyType
    , prettySpecialType0
    , prettySpecialType1
    , Pretty(pretty)
    ) where

import Data.Fix (foldFix)
import Data.Text.Short (ShortText, toText)
import LLVM.AST qualified as LLVM
import Numeric (showHex)
import Prettyprinter
    ( Doc
    , Pretty(pretty)
    , align
    , braces
    , concatWith
    , dquotes
    , encloseSep
    , flatAlt
    , group
    , hardline
    , nest
    , parens
    , (<+>)
    )

import Language.Elemental.Rewrite
import Language.Elemental.Syntax.Internal


instance Pretty (Program a) where
    pretty (Program _ decls) = concatWith declSep $ pretty <$> decls
      where
        declSep :: Doc ann -> Doc ann -> Doc ann
        declSep x y = x <> flatAlt hardline "; " <> y

instance Pretty (Decl a) where
    pretty decl = (prettyDeclHead decl <+>) . nest 4 $ case decl of
        Binding _ _ expr -> "=" <+> prettyAnnExpr0 expr
        ForeignImport _ _ _ t -> ":" <+> prettyAnnType0 t
        ForeignExport _ _ expr t
            -> prettyAnnExpr2 expr <+> ":" <+> prettyAnnType0 t
        ForeignPrimitive _ _ t -> ":" <+> prettyAnnType0 t
        ForeignAddress _ _ _ t -> ":" <+> prettyAnnType0 t

{-|
    Prettyprints a declaration, omitting any expressions and types included in
    the declaration.
-}
prettyDeclHead :: Decl a -> Doc ann
prettyDeclHead decl = case decl of
    Binding _ dname _ -> pretty dname
    ForeignImport _ dname foreignName _ -> "foreign import" <+> pretty dname
        <+> dquotes (prettyShortText foreignName)
    ForeignExport _ foreignName _ _ -> "foreign export"
        <+> dquotes (prettyShortText foreignName)
    ForeignPrimitive _ dname _ -> "foreign primitive" <+> pretty dname
    ForeignAddress _ dname addr _ -> "foreign address" <+> pretty dname
        <+> "0x" <> pretty (showHex addr "")

instance Pretty (DeclName a) where
    pretty dname = case dname of
        DeclName _ name -> pretty name

instance Pretty (AnnExpr a) where
    pretty = parens . prettyAnnExpr0

-- | Prettyprints an expression.
prettyAnnExpr0 :: AnnExpr a -> Doc ann
prettyAnnExpr0 = (.) ($ 0) . foldFix
    $ flip prettyExprF . imap (prettyType . mapFix extract) . biextract

{-|
    Prettyprints an expression. If the top level is an abstraction or a type
    abstraction, then the expression will be wrapped in parenthesis.
-}
prettyAnnExpr1 :: AnnExpr a -> Doc ann
prettyAnnExpr1 = (.) ($ 1) . foldFix
    $ flip prettyExprF . imap (prettyType . mapFix extract) . biextract

{-|
    Prettyprints an expression. If the top level is an abstraction, a type
    abstraction, an application, or a type application, then the expression will
    be wrapped in parenthesis.
-}
prettyAnnExpr2 :: AnnExpr a -> Doc ann
prettyAnnExpr2 = (.) ($ 2) . foldFix
    $ flip prettyExprF . imap (prettyType . mapFix extract) . biextract

instance Pretty Expr where
    pretty = parens . prettyExpr0

prettyExpr0 :: Expr -> Doc ann
prettyExpr0 = (.) ($ 0) . foldFix $ flip prettyExprF . imap prettyType

prettyExpr1 :: Expr -> Doc ann
prettyExpr1 = (.) ($ 1) . foldFix $ flip prettyExprF . imap prettyType

prettyExpr2 :: Expr -> Doc ann
prettyExpr2 = (.) ($ 2) . foldFix $ flip prettyExprF . imap prettyType

-- | Fold function for expressions of variable precendence.
prettyExprF :: Int -> ExprF (Int -> Doc ann) (Int -> Doc ann) -> Doc ann
prettyExprF idx = case idx of
    0 -> prettyExpr0F
    1 -> prettyExpr1F
    2 -> prettyExpr2F
    _ -> parens . prettyExpr0F

-- | Fold function for precedence 0 expressions.
prettyExpr0F :: ExprF (Int -> Doc ann) (Int -> Doc ann) -> Doc ann
prettyExpr0F ea = case ea of
    Ref {} -> prettyExpr1F ea
    Var {} -> prettyExpr1F ea
    App {} -> prettyExpr1F ea
    TypeApp {} -> prettyExpr1F ea
    Lam tx ey -> "λ" <> tx 1 <+> ey 0
    TypeLam ex -> "Λ" <+> ex 0
    InternalExpr {} -> prettyExpr1F ea

-- | Fold function for precedence 1 expressions.
prettyExpr1F :: ExprF (Int -> Doc ann) (Int -> Doc ann) -> Doc ann
prettyExpr1F ea = case ea of
    Ref {} -> prettyExpr2F ea
    Var {} -> prettyExpr2F ea
    App ef ex -> ef 1 <+> ex 2
    TypeApp ef tx -> ef 1 <+> "@" <> tx 1
    Lam {} -> prettyExpr2F ea
    TypeLam {} -> prettyExpr2F ea
    InternalExpr {} -> prettyExpr2F ea

-- | Fold function for precedence 2 expressions.
prettyExpr2F :: ExprF (Int -> Doc ann) (Int -> Doc ann) -> Doc ann
prettyExpr2F ea = case ea of
    Ref name -> pretty name
    Var idx -> pretty idx
    App {} -> prettyExprF 3 ea
    TypeApp {} -> prettyExprF 3 ea
    Lam {} -> prettyExprF 3 ea
    TypeLam {} -> prettyExprF 3 ea
    InternalExpr iex -> prettyInternalExpr iex

instance Pretty (AnnType a) where
    pretty = parens . prettyAnnType0

-- | Prettyprints a type using the given 'Int' as the precedence.
prettyAnnType :: AnnType a -> Int -> Doc ann
prettyAnnType t idx = case idx of
    0 -> prettyAnnType0 t
    1 -> prettyAnnType1 t
    _ -> pretty t

-- | Prettyprints a type.
prettyAnnType0 :: AnnType a -> Doc ann
prettyAnnType0 = (.) ($ 0) . foldFix $ flip prettyTypeF . extract

{-|
    Prettyprints a type. If the top level is an arrow or a universal
    quantification, then the type will be wrapped in parenthesis.
-}
prettyAnnType1 :: AnnType a -> Doc ann
prettyAnnType1 = (.) ($ 1) . foldFix $ flip prettyTypeF . extract

instance Pretty Type where
    pretty = parens . prettyType0

prettyType :: Type -> Int -> Doc ann
prettyType t idx = case idx of
    0 -> prettyType0 t
    1 -> prettyType1 t
    _ -> pretty t

prettyType0 :: Type -> Doc ann
prettyType0 = (.) ($ 0) . foldFix $ flip prettyTypeF

prettyType1 :: Type -> Doc ann
prettyType1 = (.) ($ 1) . foldFix $ flip prettyTypeF

-- | Fold function for types of variable precedence.
prettyTypeF :: Int -> TypeF (Int -> Doc ann) -> Doc ann
prettyTypeF idx = case idx of
    0 -> prettyType0F
    1 -> prettyType1F
    _ -> parens . prettyType0F

-- | Fold function for precedence 0 types.
prettyType0F :: TypeF (Int -> Doc ann) -> Doc ann
prettyType0F ta = case ta of
    Arrow tx ty -> tx 1 <+> "→" <+> ty 0
    Forall tx -> "∀" <+> tx 0
    TypeVar {} -> prettyType1F ta
    SpecialType st -> prettySpecialType0 st

-- | Fold function for precendence 1 types.
prettyType1F :: TypeF (Int -> Doc ann) -> Doc ann
prettyType1F ta = case ta of
    Arrow {} -> prettyTypeF 2 ta
    Forall {} -> prettyTypeF 2 ta
    TypeVar idx -> pretty idx
    SpecialType stx -> prettySpecialType1 stx

-- | Prettyprints a special type.
prettySpecialType0 :: SpecialType (Int -> Doc ann) -> Doc ann
prettySpecialType0 sta = case sta of
    IOType tx -> "IO" <+> tx 1
    PointerType pk tx -> (case pk of
        ReadPointer -> "ReadPointer"
        WritePointer -> "WritePointer"
        ) <+> tx 1
    InternalType {} -> prettySpecialType1 sta

{-|
    Prettyprints a special type. If the top level is an @IO@ type, then the
    special type will be wrapped in parenthesis.
-}
prettySpecialType1 :: SpecialType (Int -> Doc ann) -> Doc ann
prettySpecialType1 sta = case sta of
    IOType {} -> parens $ prettySpecialType0 sta
    PointerType {} -> parens $ prettySpecialType0 sta
    InternalType itx -> pretty itx

instance Pretty Name where
    pretty = prettyShortText . unName

prettyInternalExpr :: InternalExpr (Int -> Doc ann) (Int -> Doc ann) -> Doc ann
prettyInternalExpr iea = case iea of
    PureIO -> "#pureIO"
    BindIO -> "#bindIO"
    PurePrim size -> "#purei" <> pretty size
    BindPrim size v t f -> braces
        $ "#bindi" <> pretty size <+> v 2 <+> t 1 <+> f 2
    BitVector es -> prettyList' '<' '>' $ ($ 0) <$> es
    Call _ argc tret -> "#call" <> pretty argc <> prettyLlvmType tret
    IsolateBit bit size -> "#isolate" <> pretty bit <> "i" <> pretty size
    TestBit -> "#testBit"
    LoadPointer -> "#loadPointer"
    StorePointer -> "#storePointer"
    StorePrim t -> "#store" <> prettyLlvmType t
    LlvmValue v -> case v of
        LlvmUnit {} -> "()"
        LlvmOperand {} -> "{op}"
    LlvmIO {} -> "{llvm}"
    where
    prettyLlvmType :: LLVM.Type -> Doc ann
    prettyLlvmType t = case t of
        LLVM.VoidType -> "void"
        LLVM.IntegerType size -> "i" <> pretty size
        _ -> "<unknown>"

instance Pretty (InternalType a) where
    pretty ita = case ita of
        LlvmInt size -> "i" <> pretty size

-- | Prettyprints a list enclosed using the given beginning and end characters.
prettyList' :: Char -> Char -> [Doc ann] -> Doc ann
prettyList' begin end = align . group . encloseSep altBegin altEnd ", "
  where
    altBegin, altEnd :: Doc ann
    altBegin = flatAlt (pretty $ begin : " ") $ pretty begin
    altEnd = flatAlt (pretty [' ', end]) $ pretty end

-- | Prettyprints a 'ShortText'.
prettyShortText :: ShortText -> Doc ann
prettyShortText = pretty . toText
