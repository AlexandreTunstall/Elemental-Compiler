{-# LANGUAGE ImportQualifiedPost #-}

-- | Tests which verify the correctness of source location annotations.
module Location where

import Data.Functor (void)
import Data.Text qualified as T
import Hedgehog (Property, footnote, forAll, property, (===))
import Prettyprinter (defaultLayoutOptions, layoutPretty)
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
    [ testProperty "Subexpression" propSubexpression
    ]

propSubexpression :: Property
propSubexpression = property $ do
    expr <- forAll $ genExpr 0 0 5
    let src = renderStrict . layoutPretty defaultLayoutOptions
            $ prettyExpr0 expr
        parsed = parseExpr src
    subexpr <- forAll $ genSubexpr parsed
    let subsrc = isolateSpan src $ getLabel subexpr
        subparsed = parseExpr subsrc
    footnote $ "Expression: " <> T.unpack src
    footnote $ "Subexpression: " <> T.unpack subsrc
    void subexpr === void subparsed
  where
    parseExpr :: T.Text -> Expr SrcSpan
    parseExpr = either (error . errorBundlePretty) id
        . runParser (mkParser $ pExpr0 <* eof) "<gen>"

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
