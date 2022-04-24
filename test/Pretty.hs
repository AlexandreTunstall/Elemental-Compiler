{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty where

import Control.Carrier.Lift (LiftC, runM)
import Control.Monad ((>=>))
import Data.Bifunctor (bimap, first)
import Data.Map qualified as M
import Data.Text qualified as T
import Hedgehog (Property, forAllWith, property, tripping)
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Text.Megaparsec (MonadParsec(eof), errorBundlePretty, runParser)

import Gen
import Language.Elemental
import Util


prettyTests :: TestTree
prettyTests = testGroup "Pretty"
    [ testProperty "Parseable" propParseDecl
    , testProperty "ParseableUnchecked" propParseUProgram
    ]

propParseDecl :: Property
propParseDecl = property $ do
    declCode <- forAllWith (show . prettySomeDecl) $ genDecl SNil 5 $ \decl
        -> pure $ SomeDecl decl
    tripping declCode enc dec
  where
    enc :: SomeDecl -> T.Text
    enc = renderStrict . layoutPretty defaultLayoutOptions . prettySomeDecl

    dec :: T.Text -> Either String SomeDecl
    dec = first errorBundlePretty . runParser (mkParser $ pDecl <* eof) "<gen>"
        >=> tcDecl'
    
    tcDecl' :: PDecl -> Either String SomeDecl
    tcDecl' udecl = printDiags $ tcDecl M.empty SNil udecl
        $ \_ _ decl -> pure $ SomeDecl decl

propParseUProgram :: Property
propParseUProgram = property $ do
    program <- forAllWith (show . prettyUProgram) $ genUProgram 5 5
    tripping' show1 T.unpack show3 program enc dec
  where
    show1 = show . prettyUProgram
    show3 = either id show1

    enc :: UProgram -> T.Text
    enc = renderStrict . layoutPretty defaultLayoutOptions . prettyUProgram

    dec :: T.Text -> Either String UProgram
    dec = bimap errorBundlePretty stripProgram
        . runParser (mkParser $ pProgram <* eof) "<gen>"

    stripProgram :: PProgram -> UProgram
    stripProgram (P _ (UProgram decls)) = UProgram
        $ mapDeclF sndP sndP sndP stripType stripExpr . sndP <$> decls

printDiags :: DiagnosisC Diagnostic (LiftC (Either String)) a -> Either String a
printDiags = runM . runDiagnosis pure (printDiag "Error") (printDiag "Warning")

printDiag :: String -> SourceLocation -> Diagnostic -> LiftC (Either String) r
printDiag t l d = error $ show $ pretty t <> ":" <+> withSource l (pretty d)
