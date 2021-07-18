{-# LANGUAGE ImportQualifiedPost #-}

module Gen where

import Control.Monad (unless)
import Data.Fix (Fix(Fix), unFix)
import Data.Text.Short qualified as TS
import Hedgehog (Gen, MonadGen, MonadTest, PropertyT, failure, forAllWith)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prettyprinter
    ( Doc
    , PageWidth(Unbounded)
    , defaultLayoutOptions
    , group
    , layoutPageWidth
    , layoutPretty
    )
import Prettyprinter.Render.String (renderString)

import Language.Elemental


genSubexpr :: MonadGen m => AnnExpr a -> m (AnnExpr a)
genSubexpr ea = case biextract $ unFix ea of
    Ref {} -> pure ea
    Var {} -> pure ea
    App ef ex -> Gen.choice [genSubexpr ef, genSubexpr ex, pure ea]
    TypeApp ef _ -> Gen.choice [genSubexpr ef, pure ea]
    Lam _ ey -> Gen.choice [genSubexpr ey, pure ea]
    TypeLam ex -> Gen.choice [genSubexpr ex, pure ea]
    InternalExpr {} -> pure ea

genSubtype :: MonadGen m => AnnType a -> m (AnnType a)
genSubtype ta = case extract $ unFix ta of
    Arrow tx ty -> Gen.choice [genSubtype tx, genSubtype ty, pure ta]
    Forall tx -> Gen.choice [genSubtype tx, pure ta]
    TypeVar {} -> pure ta
    SpecialType stx -> case stx of
        IOType tx -> Gen.choice [genSubtype tx, pure ta]
        PointerType _ tx -> Gen.choice [genSubtype tx, pure ta]
        InternalType {} -> pure ta

genDecl :: MonadGen m => Int -> m (Decl ())
genDecl n = Gen.choice
    [ Binding () <$> genDeclName <*> (annExpr () <$> genExpr 0 0 n)
    , ForeignImport () <$> genDeclName <*> genForeignName
        <*> (annType () <$> genType 0 n)
    , ForeignExport () <$> genForeignName
        <*> (annExpr () <$> genExpr 0 0 n) <*> (annType () <$> genType 0 n)
    , ForeignPrimitive () <$> genDeclName <*> (annType () <$> genType 0 n)
    , ForeignAddress () <$> genDeclName <*> genAddr
        <*> (annType () <$> genType 0 n)
    ]
  where
    genAddr :: MonadGen m => m Integer
    genAddr = Gen.integral $ Range.constant 0 0xFFFFFFFF

genDeclName :: MonadGen m => m (DeclName ())
genDeclName = DeclName () <$> genName

genExpr :: MonadGen m => Int -> Int -> Int -> m Expr
genExpr idx tidx n = case n of
    0 -> Gen.choice $ withVar [(:\:) <$> genType tidx n <*> pure (Fix $ Var 0)]
    _ -> Gen.choice $ withVar
        [ (:$:) <$> genExpr idx tidx (n - 1) <*> genExpr idx tidx (n - 1)
        , (:@:) <$> genExpr idx tidx (n - 1) <*> genType tidx (n - 1)
        , (:\:) <$> genType tidx (n - 1) <*> genExpr (idx + 1) tidx (n - 1)
        , Fix . TypeLam <$> genExpr idx (tidx + 1) (n - 1)
        ]
  where
    withVar :: MonadGen m => [m Expr] -> [m Expr]
    withVar = case idx of
        0 -> id
        _ -> (:) $ Fix . Var <$> Gen.integral (Range.constant 0 $ idx - 1)

genType :: MonadGen m => Int -> Int -> m Type
genType tidx n = case n of
    0 -> Gen.choice $ withVar [pure . Fix . Forall . Fix $ TypeVar 0]
    _ -> Gen.choice $ withVar
        [ Fix . SpecialType <$> Gen.choice
            [ IOType <$> genType tidx (n - 1)
            , PointerType <$> genPtrKind <*> genType tidx (n - 1)
            ]
        , (:->:) <$> genType tidx (n - 1) <*> genType tidx (n - 1)
        , Fix . Forall <$> genType (tidx + 1) (n - 1)
        ]
  where
    withVar :: MonadGen m => [m Type] -> [m Type]
    withVar = case tidx of
        0 -> id
        _ -> (:) $ Fix . TypeVar <$> Gen.integral (Range.constant 0 $ tidx - 1)
    
    genPtrKind :: MonadGen m => m PointerKind
    genPtrKind = Gen.element [ReadPointer, WritePointer]

-- Do these generate surrogate pairs?

genName :: MonadGen m => m Name
genName = Name . TS.fromText <$> Gen.text (Range.constant 1 10)
    (Gen.filterT isIdentifierChar Gen.unicode)

genForeignName :: MonadGen m => m TS.ShortText
genForeignName = TS.fromText <$> Gen.text (Range.constant 1 10) Gen.unicode

forAllPretty :: (Pretty a, Monad m) => Gen a -> PropertyT m a
forAllPretty = forAllWith $ showDoc . pretty

(===) :: (Eq a, MonadTest m) => a -> a -> m ()
x === y = unless (x == y) failure

tripping
    :: (Eq (f a), Applicative f, MonadTest m)
    => a -> (a -> b) -> (b -> f a) -> m ()
tripping x enc dec = if pure x == my then pure () else failure
  where
    i = enc x
    my = dec i

-- | Renders a Doc without line breaks
showDoc :: Doc ann -> String
showDoc = renderString . layoutPretty layoutOpts . group
    where
    layoutOpts = defaultLayoutOptions
        { layoutPageWidth = Unbounded
        }

