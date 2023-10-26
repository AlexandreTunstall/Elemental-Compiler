{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Gen where

import Data.Fix (Fix(Fix), unFix)
import Data.Text.Short qualified as TS
import Data.Type.Equality ((:~:)(Refl))
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric.Natural (Natural)
import Prettyprinter (Doc)

import Language.Elemental


data SomeDecl where
    SomeDecl :: forall mt. Decl '[] mt -> SomeDecl

instance Eq SomeDecl where
    SomeDecl a == SomeDecl b = case (a, b) of
        (Binding ea, Binding eb) -> SomeExpr ea == SomeExpr eb
        (ForeignImport fna ta, ForeignImport fnb tb)
            -> fna == fnb && SomeType ta == SomeType tb
        (ForeignExport fna ea, ForeignExport fnb eb)
            -> fna == fnb && SomeExpr ea == SomeExpr eb
        (ForeignPrimitive ia, ForeignPrimitive ib) -> unifyNat False ia ib True
        (ForeignAddress aa pka ta, ForeignAddress ab pkb tb)
            -> unifyPtrKind False pka pkb
            $ aa == ab && SomeType ta == SomeType tb
        _ -> False

instance Show SomeDecl where
    show = show . prettySomeDecl

prettySomeDecl :: SomeDecl -> Doc ann
prettySomeDecl (SomeDecl decl) = prettyDecl decl

data SomeExpr where
    SomeExpr :: forall tscope scope t. Expr tscope scope t -> SomeExpr

instance Eq SomeExpr where
    SomeExpr ea == SomeExpr eb = case (ea, eb) of
        (Var va, Var vb) -> unifyNat False va vb True
        (App eax eay, App ebx eby)
            -> SomeExpr eax == SomeExpr ebx && SomeExpr eay == SomeExpr eby
        (TypeApp eax tay, TypeApp ebx tby)
            -> SomeExpr eax == SomeExpr ebx && SomeType tay == SomeType tby
        (Lam tax eay, Lam tbx eby)
            -> SomeType tax == SomeType tbx && SomeExpr eay == SomeExpr eby
        (TypeLam eax, TypeLam ebx) -> SomeExpr eax == SomeExpr ebx
        (Addr aa pka tax, Addr ab pkb tbx) -> unifyPtrKind False pka pkb
            $ aa == ab && SomeType tax == SomeType tbx
        (PureIO, PureIO) -> True
        (BindIO, BindIO) -> True
        (LoadPointer, LoadPointer) -> True
        (StorePointer, StorePointer) -> True
        _ -> False

prettySomeExpr :: Int -> SomeExpr -> Doc ann
prettySomeExpr prec (SomeExpr expr) = prettyExpr prec expr

data SomeType where
    SomeType :: forall tscope t. SType tscope t -> SomeType

instance Eq SomeType where
    SomeType ta == SomeType tb = case (ta, tb) of
        (STypeVar tva, STypeVar tvb) -> unifyNat False tva tvb True
        (SArrow tax tay, SArrow tbx tby)
            -> SomeType tax == SomeType tbx && SomeType tay == SomeType tby
        (SForall tax, SForall tbx) -> SomeType tax == SomeType tbx
        (SIOType tax, SIOType tbx) -> SomeType tax == SomeType tbx
        (SPointerType pka tax, SPointerType pkb tbx)
            -> unifyPtrKind False pka pkb $ SomeType tax == SomeType tbx
        (SBackendType lta, SBackendType ltb)
            -> unifyBackendType False lta ltb True
        _ -> False

genPSubexpr :: MonadGen m => PExpr -> m PExpr
genPSubexpr ea = case sndP1 $ unFix ea of
    URef {} -> pure ea
    UVar {} -> pure ea
    UApp ef ex -> Gen.choice [genPSubexpr ef, genPSubexpr ex, pure ea]
    UTypeApp ef _ -> Gen.choice [genPSubexpr ef, pure ea]
    ULam _ ey -> Gen.choice [genPSubexpr ey, pure ea]
    UTypeLam ex -> Gen.choice [genPSubexpr ex, pure ea]

genPSubtype :: MonadGen m => PType -> m PType
genPSubtype ta = case sndP1 $ unFix ta of
    UArrow tx ty -> Gen.choice [genPSubtype tx, genPSubtype ty, pure ta]
    UForall tx -> Gen.choice [genPSubtype tx, pure ta]
    UTypeVar {} -> pure ta
    UIOType tx -> Gen.choice [genPSubtype tx, pure ta]
    UPointerType _ tx -> Gen.choice [genPSubtype tx, pure ta]

genUProgram :: MonadGen m => Natural -> Natural -> m UProgram
genUProgram n nd
    = UProgram <$> Gen.list (Range.constant 0 $ fromIntegral n) (genUDecl nd)

genDecl
    :: MonadGen m => SList (SType 'Zero) scope -> Natural
    -> (forall mt. Decl scope mt -> m r) -> m r
genDecl scope depth cont = Gen.choice
    [ genExpr SZero scope depth $ cont . Binding
    , genForeignType $ \t -> do
        fname <- genForeignName
        cont $ ForeignImport fname t
    , genForeignType $ \t -> do
        fname <- genForeignName
        expr <- genTypedExpr SZero scope t
        cont $ ForeignExport fname expr
    , genNat (sLength primitiveExprs) $ cont . ForeignPrimitive
    , genMarshallableType $ \t -> genPtrKind $ \pk -> do
        addr <- genAddr
        cont $ ForeignAddress addr pk t
    ]
  where
    genAddr :: MonadGen m => m Address
    genAddr = fmap Address . Gen.integral $ Range.constant 0 0xFFFFFFFF

genUDecl :: MonadGen m => Natural -> m UDecl
genUDecl n = Gen.choice
    [ UBinding <$> genName <*> genUExpr 0 0 n
    , UForeignImport <$> genName <*> genForeignName <*> genUType 0 n
    , UForeignExport <$> genForeignName <*> genUExpr 0 0 n <*> genUType 0 n
    , UForeignPrimitive <$> genName <*> genUType 0 n
    , UForeignAddress <$> genName <*> genAddr <*> genUType 0 n
    ]
  where
    genAddr :: MonadGen m => m Address
    genAddr = fmap Address . Gen.integral $ Range.constant 0 0xFFFFFFFF

genExpr
    :: forall tscope scope m r. MonadGen m
    => SNat tscope -> SList (SType tscope) scope -> Natural
    -> (forall t. Expr tscope scope t -> m r) -> m r
genExpr tscope scope depth cont = Gen.choice . withVar $ case depth of
    0 -> [genType tscope depth $ cont . (:\ Var SZero)]
    _ -> [ genExpr tscope scope (depth - 1) $ \ey
            -> genExpr tscope (exprType tscope scope ey :^ scope) (depth - 1)
            $ \ex -> cont $ (exprType tscope scope ey :\ ex) :$ ey
        , genExpr (SSucc tscope) (sIncrementAll tscope SZero scope) (depth - 1)
            $ \ex -> genType tscope (depth - 1) $ \ty
            -> cont $ TypeLam ex :@ ty
        , genType tscope (depth - 1) $ \tx
            -> genExpr tscope (tx :^ scope) (depth - 1) $ \ey
            -> cont $ tx :\ ey
        , genExpr (SSucc tscope) (sIncrementAll tscope SZero scope) (depth - 1)
            $ cont . TypeLam
        ]
  where
    withVar :: [m r] -> [m r]
    withVar = case scope of
        SNil -> id
        _ :^ _ -> (:) $ genNat (sLength scope) $ cont . Var

genTypedExpr
    :: forall tscope scope t m. MonadGen m
    => SNat tscope -> SList (SType tscope) scope -> SType tscope t
    -> m (Expr tscope scope t)
genTypedExpr tscope scope t = orVar $ case t of
    STypeVar _ -> Gen.discard
    SArrow tx ty -> (tx :\) <$> genTypedExpr tscope (tx :^ scope) ty
    SForall tx -> TypeLam
        <$> genTypedExpr (SSucc tscope) (sIncrementAll tscope SZero scope) tx
    SIOType _ -> Gen.discard
    SPointerType _ _ -> Gen.discard
    SBackendType _ -> Gen.discard
  where
    orVar :: m (Expr tscope scope t) -> m (Expr tscope scope t)
    orVar m = Gen.choice [m, findVar scope $ pure . Var]

    findVar
        :: SList (SType tscope) scope'
        -> (forall idx. (CmpNat idx (Length scope') ~ 'LT, scope' !! idx ~ t)
            => SNat idx -> m r)
        -> m r
    findVar SNil _ = Gen.discard
    findVar (t' :^ ts) cont = unify' (\_ _ -> findVar ts $ cont . SSucc) t t'
        $ cont SZero

genUExpr :: MonadGen m => Natural -> Natural -> Natural -> m UExpr
genUExpr idx tidx n = case n of
    0 -> Gen.choice
        $ withVar [lam <$> genUType tidx n <*> pure (Fix $ UVar 0)]
    _ -> Gen.choice $ withVar
        [ app <$> genUExpr idx tidx (n - 1) <*> genUExpr idx tidx (n - 1)
        , typeApp <$> genUExpr idx tidx (n - 1) <*> genUType tidx (n - 1)
        , lam <$> genUType tidx (n - 1) <*> genUExpr (idx + 1) tidx (n - 1)
        , Fix . UTypeLam <$> genUExpr idx (tidx + 1) (n - 1)
        ]
  where
    lam = (.) Fix . ULam
    app = (.) Fix . UApp
    typeApp = (.) Fix . UTypeApp

    withVar :: MonadGen m => [m UExpr] -> [m UExpr]
    withVar = case idx of
        0 -> id
        _ -> (:) $ Fix . UVar <$> Gen.integral (Range.constant 0 $ idx - 1)

genType
    :: forall tscope m r. MonadGen m => SNat tscope -> Natural
    -> (forall t. SType tscope t -> m r) -> m r
genType tscope depth cont = Gen.choice . withVar $ case depth of
    0 -> [Gen.discard]
    _ -> [ genType tscope (depth - 1) $ \tx -> genType tscope (depth - 1) $ \ty
            -> cont $ SArrow tx ty
        , genType (SSucc tscope) (depth - 1) $ cont . SForall
        , genType tscope (depth - 1) $ cont . SIOType
        , genType tscope (depth - 1) $ \tx -> genPtrKind $ \pk
            -> cont $ SPointerType pk tx
        ]
  where
    withVar :: [m r] -> [m r]
    withVar = case tscope of
        SZero -> id
        SSucc _ -> (:) $ genNat tscope $ cont . STypeVar

genForeignType
    :: forall m r. MonadGen m
    => (forall t. HasForeignType t => SType 'Zero t -> m r) -> m r
genForeignType cont = Gen.choice
    [ genMarshallableRetType $ cont . SIOType
    , genMarshallableType $ \tx -> genForeignType $ cont . SArrow tx
    ]

genMarshallableType
    :: forall m r. MonadGen m
    => (forall t. (MarshallableType t, IsOpType (Marshall t) ~ 'True)
        => SType 'Zero t -> m r)
    -> m r
genMarshallableType cont = Gen.choice
    [ cont SBitType
    , genNat sup $ \size -> withProof (countBitTuple size) $ cont
        $ SForall $ SArrow (sBitTuple $ SSucc $ SSucc size) (STypeVar SZero)
    ]
  where
    sup = SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc SZero

genMarshallableRetType
    :: forall m r. MonadGen m
    => (forall t. MarshallableType t => SType 'Zero t -> m r) -> m r
genMarshallableRetType cont = Gen.choice
    [ cont SUnitType
    , genMarshallableType cont
    ]

genBackendType
    :: forall m r. MonadGen m => (forall lt. SBackendType lt -> m r) -> m r
genBackendType cont = genNat sup $ cont . SBackendInt
  where
    sup = SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc $ SSucc SZero

genUType :: MonadGen m => Natural -> Natural -> m UType
genUType tidx n = case n of
    0 -> Gen.choice $ withVar [pure . Fix . UForall . Fix $ UTypeVar 0]
    _ -> Gen.choice $ withVar
        [ Fix . UIOType <$> genUType tidx (n - 1)
        , (.) Fix . UPointerType <$> genUPtrKind <*> genUType tidx (n - 1)
        , (.) Fix . UArrow <$> genUType tidx (n - 1) <*> genUType tidx (n - 1)
        , Fix . UForall <$> genUType (tidx + 1) (n - 1)
        ]
  where
    withVar :: MonadGen m => [m UType] -> [m UType]
    withVar = case tidx of
        0 -> id
        _ -> (:) $ Fix . UTypeVar <$> Gen.integral (Range.constant 0 $ tidx - 1)
    
    genUPtrKind :: MonadGen m => m PointerKind
    genUPtrKind = Gen.element [ReadPointer, WritePointer]

genPtrKind
    :: forall m r. MonadGen m => (forall pk. SPointerKind pk -> m r) -> m r
genPtrKind cont = Gen.choice [cont SReadPointer, cont SWritePointer]

genNat
    :: forall sup m r. MonadGen m => SNat ('Succ sup)
    -> (forall idx. CmpNat idx ('Succ sup) ~ 'LT => SNat idx -> m r) -> m r
genNat (SSucc sup) cont = Gen.choice $ withProof (ltSucc sup) $ go sup
  where
    go :: forall sup'. CmpNat sup' ('Succ sup) ~ 'LT => SNat sup' -> [m r]
    go SZero = [cont SZero]
    go (SSucc sup') = withProof (ltSuccLToLT sup' (SSucc sup) Refl)
        $ cont (SSucc sup') : go sup'

-- Do these generate surrogate pairs?

genName :: MonadGen m => m Name
genName = Name . TS.fromText <$> Gen.text (Range.constant 1 10)
    (Gen.filterT isIdentifierChar Gen.unicode)

genForeignName :: MonadGen m => m ForeignName
genForeignName
    = ForeignName . TS.fromText <$> Gen.text (Range.constant 1 10) Gen.unicode
