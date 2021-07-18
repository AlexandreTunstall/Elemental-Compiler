{-# LANGUAGE ImportQualifiedPost #-}

-- | Tests which verify the correctness of source location annotations.
module Location where

import Control.Applicative
import Data.Fix (unFix)
import Data.Functor (void)
import Data.Text qualified as T
import Hedgehog (MonadTest, Property, footnote, property)
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
    decl <- forAllPretty $ genDecl 5
    let maybeCheck
            :: (Eq b, MonadTest m)
            => (a -> SrcSpan) -> (a -> b)
            -> (Decl SrcSpan -> Maybe a) -> Parser a -> m ()
        maybeCheck loc strip f p = case f parsed of
            Nothing -> pure ()
            Just x -> checkLocation' loc strip p src x
        src = docToText $ pretty decl
        parsed = parse pDecl src
    footnote $ "Full: " <> T.unpack src
    maybeCheck getLabel void getDeclName pDeclName
    maybeCheck (getBiann . unFix) stripExpr getExpr pExpr0
    maybeCheck (getAnn . unFix) stripType getType pType0
  where
    getDeclName :: Alternative f => Decl a -> f (DeclName a)
    getDeclName decl = case decl of
        Binding _ dname _ -> pure dname
        ForeignImport _ dname _ _ -> pure dname
        ForeignExport {} -> empty
        ForeignPrimitive _ dname _ -> pure dname
        ForeignAddress _ dname _ _ -> pure dname
    
    getExpr :: Alternative f => Decl a -> f (AnnExpr a)
    getExpr decl = case decl of
        Binding _ _ expr -> pure expr
        ForeignImport {} -> empty
        ForeignExport _ _ expr _ -> pure expr
        ForeignPrimitive {} -> empty
        ForeignAddress {} -> empty
    
    getType :: Alternative f => Decl a -> f (AnnType a)
    getType decl = case decl of
        Binding {} -> empty
        ForeignImport _ _ _ t -> pure t
        ForeignExport _ _ _ t -> pure t
        ForeignPrimitive _ _ t -> pure t
        ForeignAddress _ _ _ t -> pure t

propSubexpression :: Property
propSubexpression = property $ do
    expr <- forAllPretty $ genExpr 0 0 5
    checkLocation (getBiann . unFix) stripExpr
        (forAllPretty . genSubexpr) prettyExpr0 pExpr0 expr

propSubtype :: Property
propSubtype = property $ do
    t <- forAllPretty $ genType 0 5
    checkLocation (getAnn . unFix) stripType
        (forAllPretty . genSubtype) prettyType0 pType0 t

checkLocation
    :: (Eq b, MonadTest m)
    => (a -> SrcSpan) -> (a -> b)
    -> (a -> m a)
    -> (b -> Doc ann)
    -> Parser a
    -> b -> m ()
checkLocation loc strip sub enc dec x = do
    let src = docToText $ enc x
        parsed = parse dec src
    x' <- sub parsed
    footnote $ "Full: " <> T.unpack src
    checkLocation' loc strip dec src x'

checkLocation'
    :: (Eq b, MonadTest m)
    => (a -> SrcSpan) -> (a -> b)
    -> Parser a -> T.Text -> a -> m ()
checkLocation' loc strip dec src x = do
    let subsrc = isolateSpan src $ loc x
        subparsed = parse dec subsrc
    footnote $ "Selected: " <> T.unpack subsrc
    strip x === strip subparsed

docToText :: Doc ann -> T.Text
docToText = renderStrict . layoutPretty defaultLayoutOptions

parse :: Parser a -> T.Text -> a
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
