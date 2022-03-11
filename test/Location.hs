{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoStarIsType #-}

-- | Tests which verify the correctness of source location annotations.
module Location where

import Control.Applicative
import Data.Bifunctor (first)
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import Hedgehog
    (MonadTest, Property, evalEither, footnote, forAllWith, property)
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Text.Megaparsec (MonadParsec(eof), errorBundlePretty, runParser)

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
            -> Maybe a -> Parser a -> m ()
        maybeCheck loc strip parsed p = case parsed of
            Nothing -> pure ()
            Just x -> checkLocation' loc strip p src x
        src = docToText $ prettyUDecl decl
    footnote $ "Full: " <> T.unpack src
    parsed <- parse pDecl src
    footnote $ "Name: " <> maybe "<Nothing>" (show . sndP) (getDeclName parsed)
    footnote $ "Foreign Name: "
        <> maybe "<Nothing>" (show . sndP) (getForeignName parsed)
    maybeCheck fstP sndP (getDeclName parsed) pAnnName
    footnote "Name OK"
    maybeCheck fstP sndP (getForeignName parsed) pForeignName
    maybeCheck topLevelAnn stripExpr (getExpr parsed) pExpr0
    footnote "Expression OK"
    maybeCheck topLevelAnn stripType (getType parsed) pType0
  where
    getDeclName :: Alternative f => PDecl -> f AnnName
    getDeclName decl = case sndP decl of
        UBinding dname _ -> pure dname
        UForeignImport dname _ _ -> pure dname
        UForeignExport {} -> empty
        UForeignPrimitive dname _ -> pure dname
        UForeignAddress dname _ _ -> pure dname

    getForeignName :: Alternative f => PDecl -> f AnnForeignName
    getForeignName decl = case sndP decl of
        UBinding {} -> empty
        UForeignImport _ fname _ -> pure fname
        UForeignExport fname _ _ -> pure fname
        UForeignPrimitive {} -> empty
        UForeignAddress {} -> empty
    
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
    footnote $ "Full: " <> T.unpack src
    parsed <- parse dec src
    x' <- sub parsed
    checkLocation' loc strip dec src x'

checkLocation'
    :: (Eq b, MonadTest m)
    => (a -> SrcSpan) -> (a -> b)
    -> Parser a -> T.Text -> a -> m ()
checkLocation' loc strip dec src x = do
    let subsrc = isolateSpan src $ loc x
    footnote $ "Selected location: " <> show (loc x)
    footnote $ "Selected: " <> T.unpack subsrc
    subparsed <- parse dec subsrc
    strip x === strip subparsed

docToText :: Doc ann -> T.Text
docToText = renderStrict . layoutPretty defaultLayoutOptions

parse :: (MonadTest m, HasCallStack) => Parser a -> T.Text -> m a
parse p = evalEither . first (ShowString . errorBundlePretty)
    . runParser (mkParser $ p <* eof) "<gen>"

-- Hedgehog doesn't give us much control of the error message.
newtype ShowString = ShowString { unShowString :: String }

instance Show ShowString where
    show = unShowString

isolateSpan :: T.Text -> SrcSpan -> T.Text
isolateSpan src (SrcSpan begin end)
    | sourceOffset begin > sourceOffset end
        = error "start position is after end position"
    | otherwise = T.drop (sourceOffset begin) $ T.take (sourceOffset end) src

