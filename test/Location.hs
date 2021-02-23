{-# LANGUAGE ImportQualifiedPost #-}

-- | Tests which verify the correctness of source location annotations.
module Location where

import Control.Applicative
import Data.Functor (void)
import Data.Text qualified as T
import Hedgehog (MonadTest, Property, footnote, forAll, property, (===))
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Text.Megaparsec
    ( MonadParsec(eof)
    , SourcePos(sourceColumn, sourceLine)
    , errorBundlePretty
    , runParser
    , unPos
    )

import Gen
import Language.Elemental


locationTests :: TestTree
locationTests = testGroup "Location"
    [ testProperty "Declaration" propDecl
    , testProperty "Subexpression" propSubexpression
    , testProperty "Subtype" propSubtype
    ]

propDecl :: Property
propDecl = property $ do
    decl <- forAll $ genDecl 5
    let maybeCheck
            :: (Functor f, Labelled f, Eq (f ()), Show (f ()), MonadTest m)
            => (Decl SrcSpan -> Maybe (f SrcSpan)) -> Parser (f SrcSpan) -> m ()
        maybeCheck f p = case f parsed of
            Nothing -> pure ()
            Just x -> checkLocation' p src x
        src = docToText $ pretty decl
        parsed = parse pDecl src
    footnote $ "Full: " <> T.unpack src
    maybeCheck getDeclName pDeclName
    maybeCheck getExpr pExpr0
    maybeCheck getType pType0
  where
    getDeclName :: Alternative f => Decl a -> f (DeclName a)
    getDeclName decl = case decl of
        Binding _ dname _ -> pure dname
        ForeignImport _ dname _ _ -> pure dname
        ForeignExport {} -> empty
        ForeignPrimitive _ dname _ -> pure dname
        ForeignAddress _ dname _ _ -> pure dname
    
    getExpr :: Alternative f => Decl a -> f (Expr a)
    getExpr decl = case decl of
        Binding _ _ expr -> pure expr
        ForeignImport {} -> empty
        ForeignExport _ _ expr _ -> pure expr
        ForeignPrimitive {} -> empty
        ForeignAddress {} -> empty
    
    getType :: Alternative f => Decl a -> f (Type a)
    getType decl = case decl of
        Binding {} -> empty
        ForeignImport _ _ _ t -> pure t
        ForeignExport _ _ _ t -> pure t
        ForeignPrimitive _ _ t -> pure t
        ForeignAddress _ _ _ t -> pure t

propSubexpression :: Property
propSubexpression = property $ do
    expr <- forAll $ genExpr 0 0 5
    checkLocation (forAll . genSubexpr) prettyExpr0 pExpr0 expr

propSubtype :: Property
propSubtype = property $ do
    t <- forAll $ genType 0 5
    checkLocation (forAll . genSubtype) prettyType0 pType0 t

checkLocation
    :: (Functor f, Labelled f, Eq (f ()), Show (f ()), MonadTest m)
    => (f SrcSpan -> m (f SrcSpan))
    -> (f () -> Doc ann)
    -> Parser (f SrcSpan)
    -> f () -> m ()
checkLocation sub enc dec x = do
    let src = docToText $ enc x
        parsed = parse dec src
    x' <- sub parsed
    footnote $ "Full: " <> T.unpack src
    checkLocation' dec src x'

checkLocation'
    :: (Functor f, Labelled f, Eq (f ()), Show (f ()), MonadTest m)
    => Parser (f SrcSpan) -> T.Text -> f SrcSpan -> m ()
checkLocation' dec src x = do
    let subsrc = isolateSpan src $ getLabel x
        subparsed = parse dec subsrc
    footnote $ "Selected: " <> T.unpack subsrc
    void x === void subparsed

docToText :: Doc ann -> T.Text
docToText = renderStrict . layoutPretty defaultLayoutOptions

parse :: Parser (f SrcSpan) -> T.Text -> f SrcSpan
parse p = either (error . errorBundlePretty) id
    . runParser (mkParser $ p <* eof) "<gen>"

isolateSpan :: T.Text -> SrcSpan -> T.Text
isolateSpan src (SrcSpan begin end)
    | begin > end = error "start position is after end position"
    | endLinePos > length srcLines = error $ "end line " <> show endLinePos
        <> " outside of text with " <> show (length srcLines) <> " lines"
    | beginColPos > T.length beginLine + 1 = error $ "start column "
        <> show beginColPos <> " outside of line of length "
        <> show (T.length beginLine + 1)
    | endColPos > T.length endLine + 1 = error $ "end column " <> show endColPos
        <> " outside of line of length " <> show (T.length endLine + 1)
    | beginLinePos == endLinePos
        = T.drop (beginColPos - 1) $ T.take (endColPos - 1) beginLine
    | otherwise = T.concat $ T.drop (beginColPos - 1) beginLine
        : drop (beginLinePos - 1) (take (endLinePos - 1) srcLines)
        <> [T.take (endColPos - 1) endLine]
  where
    beginLinePos, endLinePos, beginColPos, endColPos :: Int
    beginLinePos = unPos $ sourceLine begin
    endLinePos = unPos $ sourceLine end
    beginColPos = unPos $ sourceColumn begin
    endColPos = unPos $ sourceColumn end

    srcLines :: [T.Text]
    srcLines = T.lines src

    beginLine, endLine :: T.Text
    beginLine = srcLines !! (beginLinePos - 1)
    endLine = srcLines !! (endLinePos - 1)
