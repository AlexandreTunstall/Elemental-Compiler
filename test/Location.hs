{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoStarIsType #-}

-- | Tests which verify the correctness of source location annotations.
module Location where

import Control.Applicative
import Data.Text qualified as T
import Hedgehog (MonadTest, Property, footnote, forAllWith, property)
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
import Util


locationTests :: TestTree
locationTests = testGroup "Location"
    [ testProperty "Declaration" propDecl
    , testProperty "Subexpression" propSubexpression
    , testProperty "Subtype" propSubtype
    ]

propDecl :: Property
propDecl = property $ do
    decl <- forAllWith (show . prettyUDecl) $ genUDecl 5
    let maybeCheck
            :: (Eq b, MonadTest m)
            => (a -> SrcSpan) -> (a -> b)
            -> (PDecl -> Maybe a) -> Parser a -> m ()
        maybeCheck loc strip f p = case f parsed of
            Nothing -> pure ()
            Just x -> checkLocation' loc strip p src x
        src = docToText $ prettyUDecl decl
        parsed = parse pDecl src
    footnote $ "Full: " <> T.unpack src
    maybeCheck fstP sndP getDeclName pAnnName
    maybeCheck topLevelAnn stripExpr getExpr pExpr0
    maybeCheck topLevelAnn stripType getType pType0
  where
    getDeclName :: Alternative f => PDecl -> f AnnName
    getDeclName decl = case sndP decl of
        UBinding dname _ -> pure dname
        UForeignImport dname _ _ -> pure dname
        UForeignExport {} -> empty
        UForeignPrimitive dname _ -> pure dname
        UForeignAddress dname _ _ -> pure dname
    
    getExpr :: Alternative f => PDecl -> f PExpr
    getExpr decl = case sndP decl of
        UBinding _ expr -> pure expr
        UForeignImport {} -> empty
        UForeignExport _ expr _ -> pure expr
        UForeignPrimitive {} -> empty
        UForeignAddress {} -> empty
    
    getType :: Alternative f => PDecl -> f PType
    getType decl = case sndP decl of
        UBinding {} -> empty
        UForeignImport _ _ t -> pure t
        UForeignExport _ _ t -> pure t
        UForeignPrimitive _ t -> pure t
        UForeignAddress _ _ t -> pure t

propSubexpression :: Property
propSubexpression = property $ do
    expr <- forAllWith (show . prettyUExpr 0) $ genUExpr 0 0 5
    checkLocation topLevelAnn stripExpr
        (forAllWith (show . prettyPExpr 0) . genPSubexpr)
        (prettyUExpr 0) pExpr0 expr

propSubtype :: Property
propSubtype = property $ do
    t <- forAllWith (show . prettyUType 0) $ genUType 0 5
    checkLocation topLevelAnn stripType
        (forAllWith (show . prettyPType 0) . genPSubtype)
        (prettyUType 0) pType0 t

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
