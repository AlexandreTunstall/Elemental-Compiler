{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Parsers for the Elemental language.
module Language.Elemental.Parser
    ( PProgram
    , PDecl
    , PExpr
    , PType
    , AnnName
    , AnnForeignName
    , AnnAddress
    , Parser
    , mkParser
    , pProgram
    , pDecl
    , pExpr0
    , pExpr1
    , pExpr2
    , pType0
    , pType1
    , pType2
    , pAnnName
    , pName
    , pForeignName
    , isIdentifierChar
    ) where

import Control.Algebra (type (:+:))
import Control.Applicative (Alternative, (<**>))
import Control.Arrow (Arrow(first, second))
import Control.Carrier.Lift (Algebra, Lift, LiftC(..), runM, sendM)
import Control.Carrier.State.Lazy
    (State, StateC(..), evalState, gets, modify, runState)
import Control.Monad (MonadPlus, void)
import Control.Monad.Fix (MonadFix)
import Data.Char (isLetter, isMark, isNumber, isPunctuation, isSymbol)
import Data.Fix (Fix(Fix))
import Data.Text (Text)
import Data.Text.Short qualified as T
import Data.Void (Void)
import Text.Megaparsec
    ( MonadParsec
        ( eof
        , getParserState
        , label
        , lookAhead
        , notFollowedBy
        , observing
        , parseError
        , takeP
        , takeWhile1P
        , takeWhileP
        , token
        , tokens
        , try
        , updateParserState
        , withRecovery
        )
    , Parsec
    , Pos
    , between
    , getOffset
    , getSourcePos
    , many
    , manyTill
    , mkPos
    , optional
    , (<|>)
    )
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as Lex

import Language.Elemental.Algebra
import Language.Elemental.AST.Decl
import Language.Elemental.AST.Expr
import Language.Elemental.AST.Type
import Language.Elemental.AST.Unchecked
import Language.Elemental.Location


-- | An unchecked program annotated with source spans.
type PProgram = SrcSpan * UProgramF PDecl

-- | An unchecked declaration annotated with source spans.
type PDecl = SrcSpan * UDeclF AnnName AnnForeignName AnnAddress PType PExpr

-- | An unchecked expression annotated with source spans.
type PExpr = Fix (K SrcSpan * UExprF PType)

-- | An unchecked type annotated with source spans.
type PType = Fix (K SrcSpan * UTypeF)

-- | A t'Name' annotated with a source span.
type AnnName = SrcSpan * Name

-- | A t'ForeignName' annotated with a source span.
type AnnForeignName = SrcSpan * ForeignName

-- | An t'Address' annotated with a source span.
type AnnAddress = SrcSpan * Address

-- | Parser monad used by the Elemental parsers.
newtype Parser a = Parser
    { getParser :: StateC (Maybe FullSourcePos, Parser ())
        (LiftC (Parsec Void Text)) a
    }
    deriving newtype
        ( Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix
        , Algebra (State (Maybe FullSourcePos, Parser ())
            :+: Lift (Parsec Void Text))
        )

{-|
    Converts a parser into a simple 'Parsec'. This should be run last on the
    whole parser to ensure that metadata such as source spans is as accurate as
    possible.
-}
mkParser :: Parser a -> Parsec Void Text a
mkParser (Parser m) = runM $ evalState (Nothing, space) m

{-|
    Converts a general space parser into a parser that also skips comments.
    Some additional bookkeeping is done for accurate source spans.
-}
mkSpace :: Parser () -> Parser ()
mkSpace p = do
    pos <- getFullSourcePos
    modify @(Maybe FullSourcePos, Parser ()) . first . const $ Just pos
    >> Lex.space p (Lex.skipLineComment "--") (Lex.skipBlockComment "{-" "-}")

-- | Whitespace parser. This parses comments and newlines.
space :: Parser ()
space = mkSpace space1

-- | Lexeme parser.
lexeme :: Parser a -> Parser a
lexeme p = gets @(Maybe FullSourcePos, Parser ()) snd >>= flip Lex.lexeme p

-- | Tracks the source span parsed by the parser and passes it to the function.
withSrcSpan :: Parser (SrcSpan -> a) -> Parser a
withSrcSpan p = do
    start <- getFullSourcePos
    modify @(Maybe FullSourcePos, Parser ()) . first $ const Nothing
    f <- p
    mspan <- gets @(Maybe FullSourcePos, Parser ()) fst
    f . SrcSpan start <$> maybe getFullSourcePos pure mspan

getFullSourcePos :: Parser FullSourcePos
getFullSourcePos = FullSourcePos <$> getSourcePos <*> getOffset

-- | Parses a text symbol as a lexeme.
symbol :: Text -> Parser Text
symbol t = gets @(Maybe FullSourcePos, Parser ()) snd >>= flip Lex.symbol t

-- | Runs a parser with a whitespace parser that enforces indentation.
indentGuard :: Parser () -> Ordering -> Pos -> Parser ()
indentGuard sc ord ref = do
    sc
    actual <- Lex.indentLevel
    if compare actual ref == ord
        then pure ()
        else modify @(Maybe FullSourcePos, Parser ()) . second . const
            . void $ Lex.incorrectIndent ord ref actual

-- | Runs a parser, requiring all parsed lexemes to be indented.
lineFold :: Parser a -> Parser a
lineFold p = do
    sc <- gets @(Maybe FullSourcePos, Parser ()) snd
    sc
    let sc' = indentGuard sc GT $ mkPos 1
    modify @(Maybe FullSourcePos, Parser ()) . second $ const sc'
    r <- p
    modify @(Maybe FullSourcePos, Parser ()) . second $ const sc
    r <$ sc

{-|
    Left associative chain. Enables parsing left associative expressions that
    would otherwise cause an infinite loop.
-}
chainl1 :: Parser a -> Parser b -> Parser (SrcSpan -> a -> b -> a) -> Parser a
chainl1 p1 p2 op = scan
  where
    {-
        "x+y+z" is parsed as "x+(y+z)" and then reassociated to "(x+y)+z".

        For accurate source spans, we parse "[x]+[y]+[z]", where square brackets
        indicate that we generate the source span for the bracketed expression.
        We then work out the correct source spans: "[[x+y]+z]".
    -}
    scan = withSrcSpan ((,) <$> p1) <**> rst
    rst = try ((\f (y, l) g (x, l') -> g (f (merge l' l) x y, merge l' l))
        <$> op <*> withSrcSpan ((,) <$> p2) <*> rst) <|> pure fst
    
    merge :: SrcSpan -> SrcSpan -> SrcSpan
    merge (SrcSpan begin _) (SrcSpan _ end) = SrcSpan begin end

-- | Annotates the parsed expression with the source span covered by the parser.
annotateExpr :: Parser (UExprF PType PExpr) -> Parser PExpr
annotateExpr p = withSrcSpan $ (\x l -> Fix $ P1 (K1 l) x) <$> p

-- -- | Annotates the parsed type with the source span covered by the parser.
annotateType :: Parser (UTypeF PType) -> Parser PType
annotateType p = withSrcSpan $ (\x l -> Fix $ P1 (K1 l) x) <$> p

-- | Parses a program, i.e. a list of declarations.
pProgram :: Parser PProgram
pProgram = (space *>) . withSrcSpan $ flip P . UProgram
    <$> many (pDecl <* optional (symbol ";"))

-- | Parses a single declaration.
pDecl :: Parser PDecl
pDecl = lineFold $ try pForeign <|> pBinding
  where
    pBinding, pForeign :: Parser PDecl
    pBinding = withSrcSpan $ do
        dname <- pAnnName
        _ <- symbol "="
        e <- pExpr0
        pure $ \l -> P l $ UBinding dname e

    pForeign = symbol "foreign" >> pForeignImport <|> pForeignExport
        <|> pForeignPrimitive <|> pForeignAddress

    pForeignImport, pForeignExport, pForeignPrimitive, pForeignAddress
        :: Parser PDecl
    pForeignImport = withSrcSpan $ do
        _ <- symbol "import"
        dname <- pAnnName
        foreignName <- pForeignName
        _ <- symbol ":"
        t <- pType0
        pure $ \l -> P l $ UForeignImport dname foreignName t

    pForeignExport = withSrcSpan $ do
        _ <- symbol "export"
        foreignName <- pForeignName
        e <- pExpr2
        _ <- symbol ":"
        t <- pType0
        pure $ \l -> P l $ UForeignExport foreignName e t

    pForeignPrimitive = withSrcSpan $ do
        _ <- symbol "primitive"
        dname <- pAnnName
        _ <- symbol ":"
        t <- pType0
        pure $ \l -> P l $ UForeignPrimitive dname t
    
    pForeignAddress = withSrcSpan $ do
        _ <- symbol "address"
        dname <- pAnnName
        addr <- pAddress
        _ <- symbol ":"
        t <- pType0
        pure $ \l -> P l $ UForeignAddress dname addr t

-- | Parses an expression.
pExpr0 :: Parser PExpr
pExpr0 = pLam <|> pTypeLam <|> pExpr1
  where
    pLam, pTypeLam :: Parser PExpr
    pLam = annotateExpr $ ULam <$> (char 'λ' *> pType2) <*> pExpr0

    pTypeLam = annotateExpr $ UTypeLam <$> (symbol "Λ" *> pExpr0)

{-|
    Parses an expression, except that a top-level abstraction or type
    abstraction must be wrapped in parenthesis.
-}
pExpr1 :: Parser PExpr
pExpr1 = try pAnyApp <|> pExpr2
  where
    pAnyApp :: Parser PExpr
    pAnyApp = chainl1 pExpr2 (pApp <|> pTypeApp) $ pure $ \l e1 er -> case er of
        Left e2 -> Fix . P1 (K1 l) $ UApp e1 e2
        Right t2 -> Fix . P1 (K1 l) $ UTypeApp e1 t2

    pApp, pTypeApp :: Parser (Either PExpr PType)
    pApp = Left <$> pExpr2
    pTypeApp = char '@' >> Right <$> pType2

{-|
    Parses an expression, except that a top-level abstraction, type abstraction,
    application, or type application must be wrapped in parenthesis.
-}
pExpr2 :: Parser PExpr
pExpr2 = between (symbol "(") (symbol ")") pExpr0 <|> try pVar <|> pRef
  where
    pVar, pRef :: Parser PExpr
    pVar = annotateExpr . lexeme $ UVar <$> Lex.decimal
    pRef = annotateExpr $ URef <$> pName

-- | Parses a type.
pType0 :: Parser PType
pType0 = pForall <|> try pArrow <|> pType1
  where
    pArrow, pForall :: Parser PType
    pArrow = annotateType $ UArrow <$> pType1 <*> (symbol "→" *> pType0)

    pForall = annotateType $ UForall <$> (symbol "∀" *> pType0)

{-|
    Parses a type, except that a top-level arrow or universal quantification
    must be wrapped in parenthesis.
-}
pType1 :: Parser PType
pType1 = pIO <|> pReadPtr <|> pWritePtr <|> pType2
  where
    pIO, pReadPtr, pWritePtr :: Parser PType
    pIO = annotateType $ UIOType <$> (symbol "IO" *> pType2)

    pReadPtr = annotateType $ UPointerType ReadPointer
        <$> (symbol "ReadPointer" *> pType2)

    pWritePtr = annotateType $ UPointerType WritePointer
        <$> (symbol "WritePointer" *> pType2)

{-|
    Parses a type, except that a top-level arrow, universal quantification, or
    special type must be wrapped in parenthesis.
-}
pType2 :: Parser PType
pType2 = between (symbol "(") (symbol ")") pType0 <|> pTypeVar
  where
    pTypeVar :: Parser PType
    pTypeVar = annotateType . lexeme $ UTypeVar <$> Lex.decimal

-- | Parses an annotated name.
pAnnName :: Parser AnnName
pAnnName = withSrcSpan $ flip P <$> pName

-- | Parses a name.
pName :: Parser Name
pName = lexeme $ Name . T.fromText <$> takeWhile1P Nothing isIdentifierChar

-- | Parses a foreign name.
pForeignName :: Parser AnnForeignName
pForeignName = withSrcSpan . lexeme
    $ (\x l -> P l . ForeignName $ T.fromString x)
        <$> (char '"' *> manyTill Lex.charLiteral (char '"'))

-- | Parses an address.
pAddress :: Parser AnnAddress
pAddress = withSrcSpan $ flip P . Address <$> pIntegral

-- | Parses an integral, accounting for the optional base prefix.
pIntegral :: Integral n => Parser n
pIntegral = lexeme $ (char '0' >> bin <|> hex <|> oct) <|> dec
  where
    bin, oct, dec, hex :: Integral n => Parser n
    bin = char 'b' >> Lex.binary
    oct = char 'o' >> Lex.octal
    dec = Lex.decimal
    hex = char 'x' >> Lex.hexadecimal

-- | Checks whether a character is legal in an identifier.
isIdentifierChar :: Char -> Bool
isIdentifierChar = \case
    '@' -> False
    '(' -> False
    ')' -> False
    c -> isLetter c || isMark c || isNumber c || isPunctuation c || isSymbol c

-- MTL boilerplate
instance MonadParsec Void Text Parser where
    parseError = sendParser . parseError
    label n (Parser (StateC m)) = Parser $ StateC $ mapLift (label n) . m
    try (Parser (StateC m)) = Parser $ StateC $ mapLift try . m
    lookAhead (Parser (StateC m)) = Parser $ StateC
        $ \s -> (,) s . snd <$> mapLift lookAhead (m s)
    notFollowedBy (Parser (StateC m)) = Parser $ StateC
        $ \s -> mapLift notFollowedBy (snd <$> m s) >> pure (s, ())
    withRecovery r (Parser (StateC m)) = Parser $ StateC
        $ \s -> mapLift (withRecovery $ runM . runState s . getParser . r) (m s)
    observing (Parser (StateC m)) = Parser $ StateC
        $ \s -> fixs s <$> mapLift observing (m s)
    eof = sendParser eof
    token test mt = sendParser $ token test mt
    tokens e ts = sendParser $ tokens e ts
    takeWhileP l f = sendParser $ takeWhileP l f
    takeWhile1P l f = sendParser $ takeWhile1P l f
    takeP l n = sendParser $ takeP l n
    getParserState = sendParser getParserState
    updateParserState = sendParser . updateParserState

-- | Sends a 'Parsec' monad in 'Parser'.
sendParser :: Parsec Void Text a -> Parser a
sendParser = sendM

-- | Lifts a mapping function into 'LiftC'.
mapLift :: (m a -> n b) -> LiftC m a -> LiftC n b
mapLift f (LiftC m) = LiftC $ f m

-- | Fixes the state, using the value in the 'Either' if possible.
fixs :: s -> Either a (s, b) -> (s, Either a b)
fixs s (Left a) = (s, Left a)
fixs _ (Right (s, b)) = (s, Right b)
{-# INLINE fixs #-}
