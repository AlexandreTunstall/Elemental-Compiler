{-# LANGUAGE ImportQualifiedPost #-}

module Pretty where

import Data.Bifunctor (Bifunctor(bimap))
import Data.Functor (void)
import Data.Text qualified as T
import Hedgehog (Property, property)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Text.Megaparsec (MonadParsec(eof), errorBundlePretty, runParser)

import Gen
import Language.Elemental


prettyTests :: TestTree
prettyTests = testGroup "Pretty" [testProperty "Parseable" propParseDecl]

propParseDecl :: Property
propParseDecl = property $ do
    decl <- forAllPretty $ genDecl 5
    tripping decl enc dec
  where
    enc :: Decl () -> T.Text
    enc = renderStrict . layoutPretty defaultLayoutOptions . pretty

    dec :: T.Text -> Either String (Decl ())
    dec = bimap errorBundlePretty void
        . runParser (mkParser $ pDecl <* eof) "<gen>"
