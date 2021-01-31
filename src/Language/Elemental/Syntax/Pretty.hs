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

{-# LANGUAGE OverloadedStrings #-}

-- | Prettyprinting for the Elemental AST.
module Language.Elemental.Syntax.Pretty
    ( prettyDeclHead
    , prettyExpr0
    , prettyExpr1
    , prettyExpr2
    , prettyType0
    , prettyType1
    , prettySpecialType0
    , prettySpecialType1
    , Pretty(pretty)
    ) where

import Data.Text.Short (ShortText, toText)
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

import Control.Carrier.IRBuilder
import Language.Elemental.Rewrite
import Language.Elemental.Syntax.Internal


instance Pretty (Program a) where
    pretty (Program _ decls) = concatWith declSep $ pretty <$> decls
      where
        declSep :: Doc ann -> Doc ann -> Doc ann
        declSep x y = x <> flatAlt hardline "; " <> y

instance Pretty (Decl a) where
    pretty decl = (prettyDeclHead decl <+>) . nest 4 $ case decl of
        Binding _ _ expr -> "=" <+> prettyExpr0 expr
        ForeignImport _ _ _ t -> ":" <+> prettyType0 t
        ForeignExport _ _ expr t -> prettyExpr2 expr <+> ":" <+> prettyType0 t
        ForeignPrimitive _ _ t -> ":" <+> prettyType0 t

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

instance Pretty (DeclName a) where
    pretty dname = case dname of
        DeclName _ name -> pretty name

instance Pretty (Expr a) where
    pretty = parens . prettyExpr0

-- | Prettyprints an expression.
prettyExpr0 :: Expr a -> Doc ann
prettyExpr0 ea = case ea of
    Ref {} -> prettyExpr1 ea
    Var {} -> prettyExpr1 ea
    App {} -> prettyExpr1 ea
    TypeApp {} -> prettyExpr1 ea
    Lam _ tx ey -> "λ" <> prettyType1 tx <+> prettyExpr0 ey
    TypeLam _ ex -> "Λ" <+> prettyExpr0 ex
    InternalExpr {} -> prettyExpr1 ea

{-|
    Prettyprints an expression. If the top level is an abstraction or a type
    abstraction, then the expression will be wrapped in parenthesis.
-}
prettyExpr1 :: Expr a -> Doc ann
prettyExpr1 ea = case ea of
    Ref {} -> prettyExpr2 ea
    Var {} -> prettyExpr2 ea
    App _ ef ex -> prettyExpr1 ef <+> prettyExpr2 ex
    TypeApp _ ef tx -> prettyExpr1 ef <+> "@" <> prettyType1 tx
    Lam {} -> prettyExpr2 ea
    TypeLam {} -> prettyExpr2 ea
    InternalExpr {} -> prettyExpr2 ea

{-|
    Prettyprints an expression. If the top level is an abstraction, a type
    abstraction, an application, or a type application, then the expression will
    be wrapped in parenthesis.
-}
prettyExpr2 :: Expr a -> Doc ann
prettyExpr2 ea = case ea of
    Ref _ name -> pretty name
    Var _ idx -> pretty idx
    App {} -> pretty ea
    TypeApp {} -> pretty ea
    Lam {} -> pretty ea
    TypeLam {} -> pretty ea
    InternalExpr _ iex -> pretty iex

instance Pretty (Type a) where
    pretty = parens . prettyType0

-- | Prettyprints a type.
prettyType0 :: Type a -> Doc ann
prettyType0 ta = case ta of
    Arrow _ tx ty -> prettyType1 tx <+> "→" <+> prettyType0 ty
    Forall _ tx -> "∀" <+> prettyType0 tx
    TypeVar {} -> prettyType1 ta
    SpecialType _ st -> prettySpecialType0 st

{-|
    Prettyprints a type. If the top level is an arrow or a universal
    quantification, then the type will be wrapped in parenthesis.
-}
prettyType1 :: Type a -> Doc ann
prettyType1 ta = case ta of
    Arrow {} -> pretty ta
    Forall {} -> pretty ta
    TypeVar _ idx -> pretty idx
    SpecialType _ stx -> prettySpecialType1 stx

instance Pretty (SpecialType a) where
    pretty = parens . prettySpecialType0

-- | Prettyprints a special type.
prettySpecialType0 :: SpecialType a -> Doc ann
prettySpecialType0 sta = case sta of
    IOType _ tx -> "IO" <+> prettyType1 tx
    InternalType {} -> prettySpecialType1 sta

{-|
    Prettyprints a special type. If the top level is an @IO@ type, then the
    special type will be wrapped in parenthesis.
-}
prettySpecialType1 :: SpecialType a -> Doc ann
prettySpecialType1 sta = case sta of
    IOType {} -> pretty sta
    InternalType _ itx -> pretty itx

instance Pretty Name where
    pretty = prettyShortText . unName

instance Pretty (InternalExpr a) where
    pretty iea = case iea of
        Unit -> "()"
        PureIO -> "#pureIO"
        BindIO -> "#bindIO"
        PurePrim size -> "#purei" <> pretty size
        BindPrim size -> "#bindi" <> pretty size
        BitVector es -> prettyList' '<' '>' $ prettyExpr0 <$> es
        Call _ argc tret -> "#call" <> pretty argc <> pretty (show tret)
        IsolateBit bit size -> "#isolate" <> pretty bit <> "i" <> pretty size
        TestBit -> "#testBit"
        LlvmOperand _ -> "{op}"
        Emit m -> braces $ "LLVM"
            <+> prettyExpr0 (run . voidRewr . fmap snd
                $ runIRBuilder emptyIRBuilder m)
      where
        voidRewr :: Applicative m => RewriterC (Expr a) m (Expr a) -> m (Expr a)
        voidRewr = runRewriter pure (const $ pure ()) (const id)

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
