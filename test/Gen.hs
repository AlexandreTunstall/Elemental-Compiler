{-# LANGUAGE ImportQualifiedPost #-}

module Gen where

import Data.Text.Short qualified as TS
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Language.Elemental


genDecl :: MonadGen m => Int -> m (Decl ())
genDecl n = Gen.choice
    [ Binding () <$> genDeclName <*> genExpr 0 0 n
    , ForeignImport () <$> genDeclName <*> genForeignName <*> genType 0 n
    , ForeignExport () <$> genForeignName <*> genExpr 0 0 n <*> genType 0 n
    , ForeignPrimitive () <$> genDeclName <*> genType 0 n
    ]

genDeclName :: MonadGen m => m (DeclName ())
genDeclName = DeclName () <$> genName

genExpr :: MonadGen m => Int -> Int -> Int -> m (Expr ())
genExpr idx tidx n = case n of
    0 -> Gen.choice $ withVar [Lam () <$> genType tidx n <*> pure (Var () 0)]
    _ -> Gen.choice $ withVar
        [ App () <$> genExpr idx tidx (n - 1) <*> genExpr idx tidx (n - 1)
        , TypeApp () <$> genExpr idx tidx (n - 1) <*> genType tidx (n - 1)
        , Lam () <$> genType tidx (n - 1) <*> genExpr (idx + 1) tidx (n - 1)
        , TypeLam () <$> genExpr idx (tidx + 1) (n - 1)
        ]
  where
    withVar :: MonadGen m => [m (Expr ())] -> [m (Expr ())]
    withVar = case idx of
        0 -> id
        _ -> (:) $ Var () <$> Gen.integral (Range.constant 0 $ idx - 1)

genType :: MonadGen m => Int -> Int -> m (Type ())
genType tidx n = case n of
    0 -> Gen.choice $ withVar [pure . Forall () $ TypeVar () 0]
    _ -> Gen.choice $ withVar
        [ SpecialType () <$> Gen.choice
            [ IOType () <$> genType tidx (n - 1)
            ]
        , Arrow () <$> genType tidx (n - 1) <*> genType tidx (n - 1)
        , Forall () <$> genType (tidx + 1) (n - 1)
        ]
  where
    withVar :: MonadGen m => [m (Type ())] -> [m (Type ())]
    withVar = case tidx of
        0 -> id
        _ -> (:) $ TypeVar () <$> Gen.integral (Range.constant 0 $ tidx - 1)

-- Do these generate surrogate pairs?

genName :: MonadGen m => m Name
genName = Name . TS.fromText <$> Gen.text (Range.constant 1 10)
    (Gen.filterT isIdentifierChar Gen.unicode)

genForeignName :: MonadGen m => m TS.ShortText
genForeignName = TS.fromText <$> Gen.text (Range.constant 1 10) Gen.unicode
