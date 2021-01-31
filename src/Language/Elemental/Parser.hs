{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Parsers for the Elemental language.
module Language.Elemental.Parser
    ( Parser
    , mkParser
    , pProgram
    , pDecl
    , pExpr0
    , pExpr1
    , pExpr2
    , pType0
    , pType1
    , pType2
    , pSpecialType
    ) where

import Control.Algebra (type (:+:))
import Control.Applicative (Alternative, liftA2, (<**>))
import Control.Arrow (Arrow(first, second))
import Control.Carrier.Lift (Algebra, Lift, LiftC(..), runM, sendM)
import Control.Carrier.State.Lazy
    (State, StateC(..), evalState, gets, modify, runState)
import Control.Monad (MonadPlus, void)
import Control.Monad.Fix (MonadFix)
import Data.Char (isLetter, isMark, isNumber, isPunctuation, isSymbol)
import Data.Text (Text)
import qualified Data.Text.Short as T
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
    , SourcePos
    , between
    , getSourcePos
    , many
    , manyTill
    , mkPos
    , optional
    , (<|>)
    )
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as Lex

import Language.Elemental.Location
import Language.Elemental.Syntax.Internal


-- | Parser monad used by the Elemental parsers.
newtype Parser a = Parser
    { getParser :: StateC (Maybe SourcePos, Parser ())
        (LiftC (Parsec Void Text)) a
    }
    deriving newtype
        ( Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix
        , Algebra (State (Maybe SourcePos, Parser ())
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
    pos <- getSourcePos
    modify @(Maybe SourcePos, Parser ()) . first . const $ Just pos
    >> Lex.space p (Lex.skipLineComment "--") (Lex.skipBlockComment "{-" "-}")

-- | Whitespace parser. This parses comments and newlines.
space :: Parser ()
space = mkSpace space1

-- | Lexeme parser.
lexeme :: Parser a -> Parser a
lexeme p = gets @(Maybe SourcePos, Parser ()) snd >>= flip Lex.lexeme p

-- | Lexeme parser with the source span for the lexeme.
lexeme' :: Parser (SrcSpan -> a) -> Parser a
lexeme' = withSrcSpan . lexeme

-- | Tracks the source span parsed by the parser and passes it to the function.
withSrcSpan :: Parser (SrcSpan -> a) -> Parser a
withSrcSpan p = do
    start <- getSourcePos
    modify @(Maybe SourcePos, Parser ()) . first $ const Nothing
    f <- p
    mspan <- gets @(Maybe SourcePos, Parser ()) fst
    f . SrcSpan start <$> maybe getSourcePos pure mspan

-- | Parses a text symbol as a lexeme.
symbol :: Text -> Parser Text
symbol t = gets @(Maybe SourcePos, Parser ()) snd >>= flip Lex.symbol t

-- | Runs a parser with a whitespace parser that enforces indentation.
indentGuard :: Parser () -> Ordering -> Pos -> Parser ()
indentGuard sc ord ref = do
    sc
    actual <- Lex.indentLevel
    if compare actual ref == ord
        then pure ()
        else modify @(Maybe SourcePos, Parser ()) . second . const
            . void $ Lex.incorrectIndent ord ref actual

-- | Runs a parser, requiring all parsed lexemes to be indented.
lineFold :: Parser a -> Parser a
lineFold p = do
    sc <- gets @(Maybe SourcePos, Parser ()) snd
    sc
    let sc' = indentGuard sc GT $ mkPos 1
    modify @(Maybe SourcePos, Parser ()) . second $ const sc'
    r <- p
    modify @(Maybe SourcePos, Parser ()) . second $ const sc
    r <$ sc

{-|
    Left associative chain. Enables parsing left associative expressions that
    would otherwise cause an infinite loop.
-}
chainl1 :: Parser a -> Parser b -> Parser (SrcSpan -> a -> b -> a) -> Parser a
chainl1 p1 p2 op = scan
  where
    scan = withSrcSpan $ p1 <**> rst
    rst = try ((\f ((y, g), l) x -> g $ f l x y)
        <$> op <*> withSrcSpan ((,) <$> liftA2 (,) p2 rst)) <|> pure const

-- | Parses a program, i.e. a list of declarations.
pProgram :: Parser (SrcSpan, [Decl SrcSpan])
pProgram = (space *>) . withSrcSpan $ flip (,)
    <$> many (pDecl <* optional (symbol ";"))

-- | Parses a single declaration.
pDecl :: Parser (Decl SrcSpan)
pDecl = lineFold $ try pForeign <|> pBinding
  where
    pBinding, pForeign :: Parser (Decl SrcSpan)
    pBinding = withSrcSpan $ do
        dname <- pDeclName
        _ <- symbol "="
        e <- pExpr0
        pure $ \l -> Binding l dname e

    pForeign = symbol "foreign"
        >> pForeignImport <|> pForeignExport <|> pForeignPrimitive

    pForeignImport, pForeignExport, pForeignPrimitive :: Parser (Decl SrcSpan)
    pForeignImport = withSrcSpan $ do
        _ <- symbol "import"
        dname <- pDeclName
        foreignName <- pForeignName
        _ <- symbol ":"
        t <- pType0
        pure $ \l -> ForeignImport l dname foreignName t

    pForeignExport = withSrcSpan $ do
        _ <- symbol "export"
        foreignName <- pForeignName
        e <- pExpr2
        _ <- symbol ":"
        t <- pType0
        pure $ \l -> ForeignExport l foreignName e t

    pForeignPrimitive = withSrcSpan $ do
        _ <- symbol "primitive"
        dname <- pDeclName
        _ <- symbol ":"
        t <- pType0
        pure $ \l -> ForeignPrimitive l dname t

-- | Parses a declaration name.
pDeclName :: Parser (DeclName SrcSpan)
pDeclName = withSrcSpan $ flip DeclName <$> pName

-- | Parses an expression.
pExpr0 :: Parser (Expr SrcSpan)
pExpr0 = pLam <|> pTypeLam <|> pExpr1
  where
    pLam, pTypeLam :: Parser (Expr SrcSpan)
    pLam = withSrcSpan $ do
        _ <- char 'λ'
        t <- pType2
        e <- pExpr0
        pure $ \l -> Lam l t e

    pTypeLam = withSrcSpan $ do
        _ <- symbol "Λ"
        e <- pExpr0
        pure $ \l -> TypeLam l e

{-|
    Parses an expression, except that a top-level abstraction or type
    abstraction must be wrapped in parenthesis.
-}
pExpr1 :: Parser (Expr SrcSpan)
pExpr1 = try pAnyApp <|> pExpr2
  where
    pAnyApp :: Parser (Expr SrcSpan)
    pAnyApp = chainl1 pExpr2 (pApp <|> pTypeApp) $ pure $ \l e1 er -> case er of
        Left e2 -> App l e1 e2
        Right t2 -> TypeApp l e1 t2

    pApp, pTypeApp :: Parser (Either (Expr SrcSpan) (Type SrcSpan))
    pApp = Left <$> pExpr2
    pTypeApp = char '@' >> Right <$> pType2

{-|
    Parses an expression, except that a top-level abstraction, type abstraction,
    application, or type application must be wrapped in parenthesis.
-}
pExpr2 :: Parser (Expr SrcSpan)
pExpr2 = between (symbol "(") (symbol ")") pExpr0 <|> try pVar <|> pRef
  where
    pVar, pRef :: Parser (Expr SrcSpan)
    pVar = lexeme' $ flip Var <$> Lex.decimal
    pRef = withSrcSpan $ flip Ref <$> pName

-- | Parses a type.
pType0 :: Parser (Type SrcSpan)
pType0 = pForall <|> try pArrow <|> pType1
  where
    pArrow, pForall :: Parser (Type SrcSpan)
    pArrow = withSrcSpan $ do
        tx <- pType1
        _ <- symbol "→"
        ty <- pType0
        pure $ \l -> Arrow l tx ty

    pForall = withSrcSpan $ flip Forall <$> (symbol "∀" *> pType0)

{-|
    Parses a type, except that a top-level arrow or universal quantification
    must be wrapped in parenthesis.
-}
pType1 :: Parser (Type SrcSpan)
pType1 = pSpecialType' <|> pType2
  where
    pSpecialType' :: Parser (Type SrcSpan)
    pSpecialType' = withSrcSpan $ flip SpecialType <$> pSpecialType

{-|
    Parses a type, except that a top-level arrow, universal quantification, or
    special type must be wrapped in parenthesis.
-}
pType2 :: Parser (Type SrcSpan)
pType2 = between (symbol "(") (symbol ")") pType0 <|> pTypeVar
  where
    pTypeVar :: Parser (Type SrcSpan)
    pTypeVar = lexeme' $ flip TypeVar <$> Lex.decimal

-- | Parses a special type.
pSpecialType :: Parser (SpecialType SrcSpan)
pSpecialType = pIO
  where
    pIO :: Parser (SpecialType SrcSpan)
    pIO = withSrcSpan $ flip IOType <$> (symbol "IO" *> pType2)

-- | Parses a name.
pName :: Parser Name
pName = lexeme $ Name . T.fromText <$> takeWhile1P Nothing isIdentifierChar

-- | Parses a foreign name.
pForeignName :: Parser T.ShortText
pForeignName = lexeme $ T.fromString
    <$> (char '"' *> manyTill Lex.charLiteral (char '"'))

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
