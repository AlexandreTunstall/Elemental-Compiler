{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-
    Using StrictData in this module gives better performance, but causes
    unresolvable incomplete pattern warnings (trying to resolve them causes
    inaccessible code warnings).
-}

module Language.Elemental.AST.Expr
    ( Expr(..)
    , Address(..)
    -- * Synonyms
    , PureIOType
    , BindIOType
    , LoadPointerType
    , StorePointerType
    , pattern (:$)
    , pattern (:@)
    , pattern (:\)
    -- * Marshalling
    , LlvmOperandType
    , IsOpType
    , sIsOpType
    , AllIsOpType
    , sAllIsOpType
    , HasForeignType(..)
    , BuildForeignType
    , sBuildForeignType
    , MarshallableType(..)
    , BitTuple
    , sBitTuple
    , ArgCount
    , sArgCount
    , Unmarshall
    , sUnmarshall
    -- * Operations
    , exprType
    , IncrementAll
    , sIncrementAll
    , SubstituteAll
    , sSubstituteAll
    , incrementExpr
    , substituteExpr
    , incrementExprType
    , substituteExprType
    -- * Proofs
    , incIncAll
    , subIncAll
    , incIns
    , incRem
    , incIdx
    , subIdx
    , incAllLen
    , subAllLen
    , incForeign
    , subForeign
    , countBitTuple
    ) where

import Data.Data
import Data.Kind qualified as Kind
import Data.Void (absurd)
import LLVM.AST qualified as LLVM
import LLVM.AST.Constant qualified as LLVM.Constant
import Numeric (showHex)
import Numeric.Natural (Natural)
import Prettyprinter
import Unsafe.Coerce qualified as Unsafe

import Control.Effect.IRBuilder
import Language.Elemental.AST.Type
import Language.Elemental.Singleton


{-|
    Expressions in the AST.

    The first additional parameter indicates the type scope and the second
    addditional parameter indicates the expression scope. These two scopes are
    completely independent and have their own de Bruijn indices.
-}
type Expr :: Nat -> [Type] -> Type -> Kind.Type
data Expr tscope scope t where
    -- | A variable referencing the expression at the given de Bruijn index.
    Var :: CmpNat idx (Length scope) ~ 'LT
        => SNat idx -> Expr tscope scope (scope !! idx)
    -- | An application of a function expression to an argument expression.
    App :: Expr tscope scope ('Arrow tx ty) -> Expr tscope scope tx
        -> Expr tscope scope ty
    -- | An application of a type function expression to an argument type.
    TypeApp
        :: Expr tscope scope ('Forall tx) -> SType tscope ty
        -> Expr tscope scope (Substitute 'Zero ty tx)
    -- | Introduces an expression of the argument type to the scope.
    Lam :: SType tscope tx -> Expr tscope (tx ': scope) ty
        -> Expr tscope scope ('Arrow tx ty)
    -- | Introduces a type to the type scope.
    TypeLam
        :: Expr ('Succ tscope) (IncrementAll 'Zero scope) tx
        -> Expr tscope scope ('Forall tx)
    -- | A pointer address. This is an internal expression.
    Addr
        :: (MarshallableType tx, IsOpType (Marshall tx) ~ 'True)
        => Address -> SPointerKind pk -> SType tscope tx
        -> Expr tscope scope ('PointerType pk tx)
    -- | A pure LLVM operand. This is an internal expression.
    LlvmOperand
        :: SLlvmType lt -> LlvmOperandType lt
        -> Expr tscope scope ('LlvmType lt)
    -- | An LLVM operand in @IO@. This is an internal expression.
    LlvmIO
        :: SLlvmType lt
        -> (forall sig m. Has IRBuilder sig m => m (LlvmOperandType lt))
        -> Expr tscope scope ('IOType ('LlvmType lt))
    -- | The @pureIO@ primitive. This is an internal expression.
    PureIO :: Expr tscope scope PureIOType
    -- | The @bindIO@ primitive. This is an internal expression.
    BindIO :: Expr tscope scope BindIOType
    -- | The @loadPointer@ primitive. This is an internal expression.
    LoadPointer :: Expr tscope scope LoadPointerType
    -- | The @storePointer@ primitive. This is an internal expression.
    StorePointer :: Expr tscope scope StorePointerType
    -- | A call of a foreign function. This is an internal expression.
    Call
        :: AllIsOpType ltargs ~ 'True => LLVM.CallableOperand -> [LLVM.Operand]
        -> SList SLlvmType ltargs -> SLlvmType ltret
        -> Expr tscope scope (BuildForeignType ltargs ltret)
    -- | Extracts a single bit from an integer. This is an internal expression.
    IsolateBit
        :: CmpNat idx size ~ 'LT => SNat idx -> SNat size -> Expr tscope scope
            ('LlvmType ('LlvmInt size) :-> 'LlvmType ('LlvmInt ('Succ 'Zero)))
    -- | Inserts a bit as the MSB of an integer. This is an internal expression.
    InsertBit :: SNat size -> Expr tscope scope
        ('LlvmType ('LlvmInt ('Succ 'Zero)) :-> 'LlvmType ('LlvmInt size)
            :-> 'LlvmType ('LlvmInt ('Succ size)))
    -- | Converts an LLVM @i1@ into a 'BitType'. This is an internal expression.
    TestBit
        :: Expr tscope scope ('LlvmType ('LlvmInt ('Succ 'Zero)))
        -> Expr tscope scope BitType

-- | Pointer addresses in the AST.
newtype Address = Address { getAddress :: Natural }
    deriving newtype (Eq, Show)

instance Pretty Address where
    pretty (Address n) = "0x" <> pretty (showHex n "")

-- | The type of the @pureIO@ primitive.
type PureIOType = 'Forall ('TypeVar 'Zero :-> 'IOType ('TypeVar 'Zero))

-- | The type of the @bindIO@ primitive.
type BindIOType
    = 'Forall ('IOType ('TypeVar 'Zero) :-> 'Forall (('TypeVar ('Succ 'Zero)
    :-> 'IOType ('TypeVar 'Zero)) :-> 'IOType ('TypeVar 'Zero)))

-- | The type of the @loadPointer@ primitive.
type LoadPointerType = 'Forall ('PointerType 'ReadPointer ('TypeVar 'Zero)
    :-> 'IOType ('TypeVar 'Zero))

-- | The type of the @storePointer@ primitive.
type StorePointerType = 'Forall ('PointerType 'WritePointer ('TypeVar 'Zero)
    :-> 'TypeVar 'Zero :-> 'IOType UnitType)

-- | Infix operator bidirectional pattern for 'App'.
pattern (:$)
    :: Expr tscope scope (tx :-> t) -> Expr tscope scope tx
    -> Expr tscope scope t
pattern ef :$ ex = App ef ex
infixl 1 :$

-- | Infix operator bidirectional pattern for 'TypeApp'.
pattern (:@)
    :: () => (t ~ Substitute 'Zero ty tx)
    => Expr tscope scope ('Forall tx) -> SType tscope ty -> Expr tscope scope t
pattern ef :@ tx = TypeApp ef tx
infixl 1 :@

-- | Infix operator bidirectional pattern for 'Lam'.
pattern (:\)
    :: () => (t ~ 'Arrow tx ty)
    => SType tscope tx -> Expr tscope (tx ': scope) ty -> Expr tscope scope t
pattern tx :\ ey = Lam tx ey
infixr 0 :\

-- | Type synonym to convert any 'LlvmType' into its compiler representation.
type LlvmOperandType :: LlvmType -> Kind.Type
type LlvmOperandType lt = If (IsOpType lt) LLVM.Operand ()

-- | Is the 'LlvmType' a legal LLVM operand type? Notably, @void@ is not.
type IsOpType :: LlvmType -> Bool
type family IsOpType lt where
    IsOpType ('LlvmInt size) = SwitchOrd (CmpNat size 'Zero) Stuck 'False 'True

-- | Singleton version of 'IsOpType'.
sIsOpType :: SLlvmType lt -> SBool (IsOpType lt)
sIsOpType (SLlvmInt size) = case sCmpNat size SZero of
    SLT -> absurd $ zeroNoLT size Refl
    SEQ -> SFalse
    SGT -> STrue

-- | Is every 'LlvmType' in a list a legal LLVM operand type?
type AllIsOpType :: [LlvmType] -> Bool
type family AllIsOpType lts where
    AllIsOpType '[] = 'True
    AllIsOpType (lt ': lts) = IsOpType lt && AllIsOpType lts

-- | Singleton version of 'AllIsOpType'.
sAllIsOpType :: SList SLlvmType lts -> SBool (AllIsOpType lts)
sAllIsOpType SNil = STrue
sAllIsOpType (lt :^ lts) = sIsOpType lt &&^ sAllIsOpType lts

-- | The type has an isomorphic foreign type.
type HasForeignType :: Type -> Kind.Constraint
class AllIsOpType (ForeignArgs t) ~ 'True => HasForeignType t where
    -- | The argument 'LlvmType' of the foreign type.
    type ForeignArgs t :: [LlvmType]

    -- | Singleton version of 'ForeignArgs'.
    sForeignArgs :: SType tscope t -> SList SLlvmType (ForeignArgs t)

    -- | The return 'LlvmType' of the foreign type.
    type ForeignRet t :: LlvmType

    -- | Singleton version of 'ForeignRet'.
    sForeignRet :: SType tscope t -> SLlvmType (ForeignRet t)

    -- | Wraps the foreign type into the native type.
    wrapImport
        :: SNat tscope -> SList (SType tscope) scope
        -> SType tscope t -> Expr tscope scope (ForeignType t)
        -> Expr tscope scope t
    
    -- | Wraps the native type into the foreign type.
    wrapExport
        :: SNat tscope -> SList (SType tscope) scope
        -> SType tscope t -> Expr tscope scope t
        -> Expr tscope scope (ForeignType t)

instance MarshallableType t => HasForeignType ('IOType t) where
    type ForeignArgs ('IOType _) = '[]
    sForeignArgs _ = SNil

    type ForeignRet ('IOType t) = Marshall t
    sForeignRet (SIOType t) = sMarshall t

    wrapImport tscope scope (SIOType t) x = BindIO :@ t' :$ x :@ t :$ (t'
        :\ PureIO :@ t
        :$ marshallIn tscope (t' :^ scope) t (Var SZero))
      where
        t' = SLlvmType $ sMarshall t

    wrapExport tscope scope (SIOType (t :: SType tscope tx)) x
        = withProof (subIncElim tscope SZero t' t Refl)
        $ BindIO :@ t :$ x :@ t' :$ (t :\ PureIO :@ t'
            :$ marshallOut tscope (t :^ scope) t (Var SZero))
      where
        t' :: SType tscope ('LlvmType (Marshall tx))
        t' = SLlvmType $ sMarshall t

instance (MarshallableType tx, IsOpType (Marshall tx) ~ 'True
    , HasForeignType ty) => HasForeignType (tx :-> ty)
  where
    type ForeignArgs (tx :-> ty) = Marshall tx ': ForeignArgs ty
    sForeignArgs (SArrow tx ty) = sMarshall tx :^ sForeignArgs ty

    type ForeignRet (_ :-> ty) = ForeignRet ty
    sForeignRet (SArrow _ ty) = sForeignRet ty

    wrapImport tscope scope (SArrow tx ty) x = tx
        :\ wrapImport tscope (tx :^ scope) ty
            ( withProof (insZeroP tx scope)
            $ incrementExpr tscope scope SZero tx x
                :$ marshallOut tscope (tx :^ scope) tx (Var SZero)
            )
    
    wrapExport tscope scope (SArrow tx ty) x = tx'
        :\ wrapExport tscope (tx' :^ scope) ty
            (withProof (insZeroP tx' scope)
            $ incrementExpr tscope scope SZero tx' x
                :$ marshallIn tscope (tx' :^ scope) tx (Var SZero)
            )
      where
        tx' = SLlvmType $ sMarshall tx

-- | The foreign type corresponding to a native type.
type ForeignType t = BuildForeignType (ForeignArgs t) (ForeignRet t)

-- | Builds a type from a list of argument 'LlvmType' and a return 'LlvmType'.
type BuildForeignType :: [LlvmType] -> LlvmType -> Type
type family BuildForeignType ltargs ltret where
    BuildForeignType '[] ltret = 'IOType ('LlvmType ltret)
    BuildForeignType (ltarg ': ltargs) ltret
        = 'LlvmType ltarg :-> BuildForeignType ltargs ltret

-- | Singleton version of 'BuildForeignType'.
sBuildForeignType
    :: SList SLlvmType ltargs -> SLlvmType ltret
    -> SType tscope (BuildForeignType ltargs ltret)
sBuildForeignType SNil ltret = SIOType $ SLlvmType ltret
sBuildForeignType (ltarg :^ ltargs) ltret
    = SLlvmType ltarg :-> sBuildForeignType ltargs ltret

-- | The type is isomorphic to and can be marshalled to and from an 'LlvmType'.
type MarshallableType :: Type -> Kind.Constraint
class t ~ Unmarshall (Marshall t) => MarshallableType t where
    -- | The 'LlvmType' corresponding to a native type.
    type Marshall t :: LlvmType

    -- | Singleton version of 'Marshall'.
    sMarshall :: SType scope t -> SLlvmType (Marshall t)

    -- | Marshalls an expression from the 'LlvmType' to the native type.
    marshallIn
        :: SNat tscope -> SList (SType tscope) scope -> SType tscope t
        -> Expr tscope scope ('LlvmType (Marshall t))
        -> Expr tscope scope t
    
    -- | Marshalls an expression from the native type to the 'LlvmType'.
    marshallOut
        :: SNat tscope -> SList (SType tscope) scope -> SType tscope t
        -> Expr tscope scope t
        -> Expr tscope scope ('LlvmType (Marshall t))
    
    -- | Proof that 'Increment' is a no-op, i.e. the type is closed.
    incMarshall
        :: SNat idx -> SType tscope t
        -> t :~: Increment idx t
    
    -- | Proof that 'Substitute' is a no-op, i.e. the type is closed.
    subMarshall
        :: SNat idx -> proxy tsub -> SType tscope t
        -> t :~: Substitute idx tsub t

instance MarshallableType UnitType where
    type Marshall UnitType = 'LlvmInt 'Zero
    sMarshall _ = SLlvmInt SZero

    marshallIn _ _ _ _ = TypeLam $ STypeVar SZero :\ Var SZero
    
    marshallOut _ _ _ _ = LlvmOperand (SLlvmInt SZero) ()

    incMarshall _ _ = Refl

    subMarshall _ _ _ = Refl

instance MarshallableType BitType where
    type Marshall BitType = 'LlvmInt ('Succ 'Zero)
    sMarshall _ = SLlvmInt $ SSucc SZero

    marshallIn _ _ _ = TestBit

    marshallOut _ _ _ x = x :@ SLlvmType lt
        :$ LlvmOperand lt (LLVM.ConstantOperand $ LLVM.Constant.Int 1 1)
        :$ LlvmOperand lt (LLVM.ConstantOperand $ LLVM.Constant.Int 1 0)
      where
        lt = SLlvmInt $ SSucc SZero

    incMarshall _ _ = Refl

    subMarshall _ _ _ = Refl

instance (t ~ BitTuple (ArgCount t), ArgCount t ~ 'Succ _n)
    => MarshallableType ('Forall ((BitType :-> t) :-> 'TypeVar 'Zero))
  where
    type Marshall ('Forall ((BitType :-> t) :-> 'TypeVar 'Zero))
        = 'LlvmInt ('Succ (ArgCount t))
    sMarshall (SForall (SArrow t _)) = SLlvmInt $ sArgCount t

    marshallIn tscope scope t x = TypeLam $ tx :\ withProof (ltSucc size)
        ( withProof (insZeroP tx scope')
        $ marshallTuple (SSucc tscope) (tx :^ scope') size size
            ( withProof (insZeroP tx scope')
            $ incrementExpr (SSucc tscope) scope' SZero tx
            $ incrementExprType tscope scope SZero x
            )
        $ Var SZero
        )
      where
        size = sArgCount tx
        SForall (SArrow tx _) = t
        scope' = sIncrementAll tscope SZero scope

        marshallTuple
            :: forall tscope scope n size. (CmpNat n ('Succ size) ~ 'LT)
            => SNat ('Succ tscope) -> SList (SType ('Succ tscope)) scope
            -> SNat n -> SNat size
            -> Expr ('Succ tscope) scope ('LlvmType ('LlvmInt size))
            -> Expr ('Succ tscope) scope (BitTuple n)
            -> Expr ('Succ tscope) scope ('TypeVar 'Zero)
        marshallTuple _ _ SZero _ _ er = er
        marshallTuple tsc sc (SSucc idx) size' ex er
            = withProof (ltSuccLToLT idx (SSucc size') Refl)
            $ marshallTuple tsc sc idx size' ex
            $ er :$ marshallIn tsc sc SBitType (IsolateBit idx size' :$ ex)
    
    marshallOut tscope scope t x = x :@ SLlvmType (sMarshall t)
        :$ marshallTuple tscope scope size (const $ const id)
      where
        size = sArgCount tx
        SForall (SArrow tx _) = t

        marshallTuple
            :: SNat tscope -> SList (SType tscope) scope
            -> SNat size
            -> (forall scope'. SList (SType tscope) scope'
                -> (forall t2. Expr tscope scope t2 -> Expr tscope scope' t2)
                -> Expr tscope scope' ('LlvmType ('LlvmInt size))
                -> Expr tscope scope' tr)
            -> Expr tscope scope (Substitute 'Zero tr (BitTuple size))
        marshallTuple _ sc SZero f = f sc id $ LlvmOperand (SLlvmInt SZero) ()
        marshallTuple tsc sc (SSucc size') f = SBitType
            :\ marshallTuple tsc (SBitType :^ sc) size' (\sc' inc ex -> f sc'
                (withProof (insZero @BitType sc)
                    $ inc . incrementExpr tsc sc SZero SBitType)
                $ InsertBit size'
                :$ marshallOut tsc sc' SBitType (inc $ Var SZero) :$ ex
                )

    incMarshall (_ :: _proxy idx) t = withProof (incBt $ sArgCount tx) Refl
      where
        SForall (SArrow (SArrow _ (SArrow _ tx)) _) = t

        incBt
            :: SNat size
            -> Increment ('Succ idx) (BitTuple size) :~: BitTuple size
        incBt SZero = Refl
        incBt (SSucc size) = withProof (incBt size) Refl

    subMarshall (_ :: SNat idx) (_ :: _p tsub) t
        = withProof (subBt $ sArgCount tx) Refl
      where
        SForall (SArrow (SArrow _ (SArrow _ tx)) _) = t

        subBt
            :: SNat size
            -> Substitute ('Succ idx) (Increment 'Zero tsub) (BitTuple size)
                :~: BitTuple size
        subBt SZero = Refl
        subBt (SSucc size) = withProof (subBt size) Refl

-- | Constructs a tuple of the given number of 'BitType'.
type BitTuple :: Nat -> Type
type family BitTuple n = r | r -> n where
    BitTuple 'Zero = 'TypeVar 'Zero
    BitTuple ('Succ n) = BitType :-> BitTuple n

-- | Singleton version of 'BitTuple'.
sBitTuple :: SNat n -> SType ('Succ tscope) (BitTuple n)
sBitTuple SZero = STypeVar SZero
sBitTuple (SSucc n) = SBitType :-> sBitTuple n

-- | Counts the number of arguments in the type.
type ArgCount :: Type -> Nat
type family ArgCount t where
    ArgCount (_ :-> tr) = 'Succ (ArgCount tr)
    ArgCount _ = 'Zero

-- | Singleton version of 'ArgCount'.
sArgCount :: SType tscope t -> SNat (ArgCount t)
sArgCount (STypeVar _) = SZero
sArgCount (SArrow _ tr) = SSucc $ sArgCount tr
sArgCount (SForall _) = SZero
sArgCount (SIOType _) = SZero
sArgCount (SPointerType _ _) = SZero
sArgCount (SLlvmType _) = SZero

-- | Converts an 'LlvmType' to an isomorphic native type. Inverse of 'Marshall'.
type Unmarshall :: LlvmType -> Type
type family Unmarshall lt where
    Unmarshall ('LlvmInt 'Zero) = 'Forall ('TypeVar 'Zero :-> 'TypeVar 'Zero)
    Unmarshall ('LlvmInt ('Succ 'Zero)) = BitType
    Unmarshall ('LlvmInt size) = 'Forall (BitTuple size :-> 'TypeVar 'Zero)

-- | Singleton version of 'Unmarshall'.
sUnmarshall :: SLlvmType lt -> SType tscope (Unmarshall lt)
sUnmarshall (SLlvmInt SZero) = SForall $ STypeVar SZero :-> STypeVar SZero
sUnmarshall (SLlvmInt (SSucc SZero))
    = SForall $ STypeVar SZero :-> STypeVar SZero :-> STypeVar SZero
sUnmarshall (SLlvmInt size@(SSucc (SSucc _)))
    = SForall $ sBitTuple size :-> STypeVar SZero

-- | Gets the type of an expression.
exprType
    :: forall tscope scope t. SNat tscope -> SList (SType tscope) scope
    -> Expr tscope scope t -> SType tscope t
exprType tscope scope = \case
    Var vidx -> scope !!^ vidx
    App ef _ -> case exprType tscope scope ef of
        SArrow _ ty -> ty
    TypeApp ef tx -> case exprType tscope scope ef of
        SForall tf -> sSubstitute tscope SZero tx tf
    Lam tx ey -> SArrow tx $ exprType tscope (tx :^ scope) ey
    TypeLam ex -> SForall
        $ exprType (SSucc tscope) (sIncrementAll tscope SZero scope) ex
    Addr _ pk tx -> SPointerType pk tx
    LlvmOperand lt _ -> SLlvmType lt
    LlvmIO lt _ -> SIOType $ SLlvmType lt
    PureIO -> SForall $ STypeVar SZero :-> SIOType (STypeVar SZero)
    BindIO -> SForall $ SIOType (STypeVar SZero)
        :-> SForall ((STypeVar (SSucc SZero) :-> SIOType (STypeVar SZero))
            :-> SIOType (STypeVar SZero))
    LoadPointer -> SForall $ SPointerType SReadPointer (STypeVar SZero)
            :-> SIOType (STypeVar SZero)
    StorePointer -> SForall $ SPointerType SWritePointer (STypeVar SZero)
            :-> STypeVar SZero :-> SIOType SUnitType
    Call _ _ ltargs ltret -> sBuildForeignType ltargs ltret
    IsolateBit _ size
        -> SLlvmType (SLlvmInt size) :-> SLlvmType (SLlvmInt $ SSucc SZero)
    InsertBit size -> SLlvmType (SLlvmInt $ SSucc SZero)
        :-> SLlvmType (SLlvmInt size)
        :-> SLlvmType (SLlvmInt (SSucc size))
    TestBit _ -> SBitType

-- | Increments every type in a list.
type IncrementAll :: Nat -> [Type] -> [Type]
type family IncrementAll idx ts where
    IncrementAll _ '[] = '[]
    IncrementAll idx (t ': ts) = Increment idx t ': IncrementAll idx ts

-- | Singleton version of 'IncrementAll'.
sIncrementAll
    :: SNat scope -> SNat idx -> SList (SType scope) ts
    -> SList (SType ('Succ scope)) (IncrementAll idx ts)
sIncrementAll _ _ SNil = SNil
sIncrementAll scope idx (t :^ ts)
    = sIncrement scope idx t :^ sIncrementAll scope idx ts

-- | Substitutes every type in a list.
type SubstituteAll :: Nat -> Type -> [Type] -> [Type]
type family SubstituteAll idx tsub ts where
    SubstituteAll _ _ '[] = '[]
    SubstituteAll idx tsub (t ': ts)
        = Substitute idx tsub t ': SubstituteAll idx tsub ts

-- | Singleton version of 'SubstituteAll'.
sSubstituteAll
    :: CmpNat idx ('Succ tscope) ~ 'LT => SNat tscope
    -> SNat idx -> SType tscope tsub -> SList (SType ('Succ tscope)) ts
    -> SList (SType tscope) (SubstituteAll idx tsub ts)
sSubstituteAll _ _ _ SNil = SNil
sSubstituteAll tscope idx tsub (t :^ ts)
    = sSubstitute tscope idx tsub t :^ sSubstituteAll tscope idx tsub ts


-- | Introduces a variable at the given index.
incrementExpr
    :: forall tscope scope idx tnew t proxy. SNat tscope
    -> SList (SType tscope) scope
    -> SNat idx -> proxy tnew -> Expr tscope scope t
    -> Expr tscope (Insert idx tnew scope) t
incrementExpr tscope scope idx tnew = \case
    Var vidx -> withProof (insSuccLen idx tnew scope)
        $ withProof (idxIns vidx idx tnew scope Refl)
        $ withProof (flipCmpNat idx vidx)
        $ withProof (cmpSuccStepR vidx (sLength scope) Refl)
        $ case sCmpNat idx vidx of
            SLT -> Var (SSucc vidx)
            SEQ -> Var (SSucc vidx)
            SGT -> Var vidx
    App ef ex -> App
        (incrementExpr tscope scope idx tnew ef)
        (incrementExpr tscope scope idx tnew ex)
    TypeApp ef tx -> TypeApp (incrementExpr tscope scope idx tnew ef) tx
    Lam tx ey -> Lam tx $ incrementExpr tscope (tx :^ scope) (SSucc idx) tnew ey
    TypeLam ex -> TypeLam $ withProof (incIns scope idx tnew)
        $ incrementExpr (SSucc tscope) (sIncrementAll tscope SZero scope)
            idx (Proxy @(Increment 'Zero tnew)) ex
    Addr addr pk tx -> Addr addr pk tx
    LlvmOperand lt op -> LlvmOperand lt op
    LlvmIO lt op -> LlvmIO lt op
    PureIO -> PureIO
    BindIO -> BindIO
    LoadPointer -> LoadPointer
    StorePointer -> StorePointer
    Call fop aops ltargs ltret -> Call fop aops ltargs ltret
    IsolateBit bidx size -> IsolateBit bidx size
    InsertBit size -> InsertBit size
    TestBit ex -> TestBit $ incrementExpr tscope scope idx tnew ex

-- | Substitutes a variable at the given index.
substituteExpr
    :: forall tscope scope idx t. CmpNat idx (Length scope) ~ 'LT
    => SNat tscope -> SList (SType tscope) scope -> SNat idx
    -> Expr tscope (Remove idx scope) (scope !! idx)
    -> Expr tscope scope t
    -> Expr tscope (Remove idx scope) t
substituteExpr tscope scope idx sub = \case
    Var (vidx :: SNat vidx) -> case sCmpNat idx vidx of
        SLT -> withProof (flipCmpNat idx vidx)
            $ withProof (ltRightPredSucc idx vidx Refl)
            $ withProof (idxRem vidx idx scope Refl Refl)
            $ withProof (remPredLen idx scope)
            $ Var $ sPred vidx
        SEQ -> withProof (eqToRefl idx vidx Refl) sub
        SGT -> withProof (flipCmpNat idx vidx)
            $ withProof (ltRightPredSucc idx (sLength scope) Refl)
            $ withProof (ltTrans vidx idx (sPred $ sLength scope) Refl Refl)
            $ withProof (idxRem vidx idx scope Refl Refl)
            $ withProof (remPredLen idx scope)
            $ withProof (selfEq $ sPred $ sLength scope)
            $ withProof (eqToRefl (sPred $ sLength scope)
                (sLength $ sRemove idx scope) Refl)
            $ Var vidx
    App ef ex -> App
        (substituteExpr tscope scope idx sub ef)
        (substituteExpr tscope scope idx sub ex)
    TypeApp ef tx -> TypeApp (substituteExpr tscope scope idx sub ef) tx
    Lam tx ey -> withProof (insZeroP tx $ sRemove idx scope)
        $ Lam tx $ substituteExpr tscope (tx :^ scope) (SSucc idx)
            (incrementExpr tscope (sRemove idx scope) SZero tx sub) ey
    TypeLam ex -> withProof (incRem idx scope)
        $ withProof (incIdx idx SZero scope Refl)
        $ withProof (incAllLen SZero scope) $ TypeLam
        $ substituteExpr (SSucc tscope) (sIncrementAll tscope SZero scope) idx
            (incrementExprType tscope (sRemove idx scope) SZero sub) ex
    Addr addr pk tx -> Addr addr pk tx
    LlvmOperand lt op -> LlvmOperand lt op
    LlvmIO lt op -> LlvmIO lt op
    PureIO -> PureIO
    BindIO -> BindIO
    LoadPointer -> LoadPointer
    StorePointer -> StorePointer
    Call fop aops ltargs ltret -> Call fop aops ltargs ltret
    IsolateBit bidx size -> IsolateBit bidx size
    InsertBit size -> InsertBit size
    TestBit ex -> TestBit $ substituteExpr tscope scope idx sub ex

-- | Introduces a type variable at the given type index.
incrementExprType
    :: forall tscope scope idx t. CmpNat idx ('Succ tscope) ~ 'LT
    => SNat tscope -> SList (SType tscope) scope
    -> SNat idx -> Expr tscope scope t
    -> Expr ('Succ tscope) (IncrementAll idx scope) (Increment idx t)
incrementExprType tscope scope idx = \case
    Var vidx -> withProof (incIdx vidx idx scope Refl)
        $ withProof (incAllLen idx scope)
        $ Var vidx
    App ef ex -> App
        (incrementExprType tscope scope idx ef)
        (incrementExprType tscope scope idx ex)
    TypeApp ef tx -> case exprType tscope scope ef of
        SForall tf
            -> withProof (incSub tscope SZero idx tx tf Refl Refl)
            $ TypeApp
                (incrementExprType tscope scope idx ef)
                (sIncrement tscope idx tx)
    Lam tx ey -> Lam (sIncrement tscope idx tx)
        $ incrementExprType tscope (tx :^ scope) idx ey
    TypeLam ex -> TypeLam $ withProof (incIncAll tscope idx scope)
        $ incrementExprType (SSucc tscope) (sIncrementAll tscope SZero scope)
            (SSucc idx) ex
    Addr addr pk tx -> withProof (incMarshall idx tx)
        $ Addr addr pk $ sIncrement tscope idx tx
    LlvmOperand lt op -> LlvmOperand lt op
    LlvmIO lt op -> LlvmIO lt op
    PureIO -> PureIO
    BindIO -> BindIO
    LoadPointer -> LoadPointer
    StorePointer -> StorePointer
    Call fop aops ltargs ltret -> withProof (incForeign idx ltargs ltret)
        $ Call fop aops ltargs ltret
    IsolateBit bidx size -> IsolateBit bidx size
    InsertBit size -> InsertBit size
    TestBit ex -> TestBit $ incrementExprType tscope scope idx ex

-- | Substitutes a type variable at the given type index.
substituteExprType
    :: forall tscope scope idx t tsub. CmpNat idx ('Succ tscope) ~ 'LT
    => SNat tscope -> SList (SType ('Succ tscope)) scope
    -> SNat idx -> SType tscope tsub -> Expr ('Succ tscope) scope t
    -> Expr tscope (SubstituteAll idx tsub scope) (Substitute idx tsub t)
substituteExprType tscope scope idx tsub = withProofs $ \case
    Var vidx -> withProof (subAllLen idx tsub scope)
        $ withProof (subIdx vidx idx tsub scope Refl)
        $ Var vidx
    App ef ex -> App
        (substituteExprType tscope scope idx tsub ef)
        (substituteExprType tscope scope idx tsub ex)
    TypeApp ef tx -> case exprType (SSucc tscope) scope ef of
        SForall tf
            -> withProof (subSub tscope SZero idx tsub tx tf Refl Refl)
            $ TypeApp
                (substituteExprType tscope scope idx tsub ef)
                (sSubstitute tscope idx tsub tx)
    Lam tx ey -> Lam (sSubstitute tscope idx tsub tx)
        $ substituteExprType tscope (tx :^ scope) idx tsub ey
    TypeLam ex -> TypeLam $ substituteExprType (SSucc tscope)
        (sIncrementAll (SSucc tscope) SZero scope) (SSucc idx)
        (sIncrement tscope SZero tsub) ex
    Addr addr pk tx -> withProof (subMarshall idx tsub tx)
        $ Addr addr pk $ sSubstitute tscope idx tsub tx
    LlvmOperand lt op -> LlvmOperand lt op
    LlvmIO lt op -> LlvmIO lt op
    PureIO -> PureIO
    BindIO -> BindIO
    LoadPointer -> LoadPointer
    StorePointer -> StorePointer
    Call fop aops ltargs ltret -> withProof (subForeign idx tsub ltargs ltret)
        $ Call fop aops ltargs ltret
    IsolateBit bidx size -> IsolateBit bidx size
    InsertBit size -> InsertBit size
    TestBit ex -> TestBit $ substituteExprType tscope scope idx tsub ex
  where
    {-
        Adding 
            , Substitute 0 (Substitute idx tsub ty)
                (Substitute (Succ idx) (Increment 0 tsub) tx) ~ t
        to the type of 'withProofs' causes a GHC panic (8.10.7).
        (Note that tx and ty are out of scope.)
    -}
    withProofs
        :: (SubstituteAll ('Succ idx) (Increment 'Zero tsub)
            (IncrementAll 'Zero scope)
            ~ IncrementAll 'Zero (SubstituteAll idx tsub scope)
            => Expr ('Succ tscope) scope t -> r)
        -> Expr ('Succ tscope) scope t -> r
    withProofs cont t
        = withProof (subIncAll tscope idx tsub scope Refl)
        $ cont t

-- | Proof for transposing two 'IncrementAll'.
incIncAll
    :: SNat tscope -> SNat idx -> SList (SType tscope) scope
    -> IncrementAll ('Succ idx) (IncrementAll 'Zero scope)
        :~: IncrementAll 'Zero (IncrementAll idx scope)
incIncAll _ _ SNil = Refl
incIncAll tscope idx (t :^ scope) = withProof (incIncAll tscope idx scope)
    $ withProof (incInc tscope idx SZero t Refl) Refl
{-# RULES "Proof/incIncAll" incIncAll
    = \_ _ _ -> Unsafe.unsafeCoerce Refl #-}
{-# INLINE [1] incIncAll #-}

-- | Proof for transposing 'SubstituteAll' and 'IncrementAll'.
subIncAll
    :: SNat tscope -> SNat idx -> SType tscope tsub
    -> SList (SType ('Succ tscope)) scope
    -> CmpNat idx ('Succ tscope) :~: 'LT
    -> SubstituteAll ('Succ idx) (Increment 'Zero tsub)
        (IncrementAll 'Zero scope)
        :~: IncrementAll 'Zero (SubstituteAll idx tsub scope)
subIncAll _ _ _ SNil _ = Refl
subIncAll tscope idx tsub (t :^ scope) lt
    = withProof (subIncAll tscope idx tsub scope lt)
    $ withProof (subInc tscope idx SZero tsub t lt Refl) Refl
{-# RULES "Proof/subIncAll" subIncAll
    = \_ _ _ _ -> Unsafe.unsafeCoerce #-}
{-# INLINE [1] subIncAll #-}

-- | Proof for transposing 'IncrementAll' and 'Insert'.
incIns
    :: SList (SType tscope) scope -> SNat idx -> proxy tnew
    -> IncrementAll 'Zero (Insert idx tnew scope)
        :~: Insert idx (Increment 'Zero tnew) (IncrementAll 'Zero scope)
incIns SNil _ _ = Refl
incIns (_ :^ ts) idx tnew = case sCmpNat idx SZero of
    SLT -> absurd $ zeroNoLT idx Refl
    SEQ -> Refl
    SGT -> withProof (flipCmpNat idx SZero)
        $ withProof (ltRightPredSucc SZero idx Refl)
        $ withProof (incIns ts (sPred idx) tnew) Refl
{-# RULES "Proof/incIns" incIns
    = \_ _ _ -> Unsafe.unsafeCoerce Refl #-}
{-# INLINE [1] incIns #-}

-- | Proof for transposing 'IncrementAll' and 'Remove'.
incRem
    :: CmpNat idx (Length scope) ~ 'LT => SNat idx -> SList proxy2 scope
    -> IncrementAll 'Zero (Remove idx scope)
        :~: Remove idx (IncrementAll 'Zero scope)
incRem idx SNil = absurd $ zeroNoLT idx Refl
incRem idx (_ :^ scope) = case sCmpNat idx SZero of
    SLT -> absurd $ zeroNoLT idx Refl
    SEQ -> Refl
    SGT -> withProof (flipCmpNat idx SZero)
        $ withProof (ltRightPredSucc SZero idx Refl)
        $ withProof (incRem (sPred idx) scope) Refl
{-# RULES "Proof/incRem" incRem
    = \_ _ -> Unsafe.unsafeCoerce Refl #-}
{-# INLINE [1] incRem #-}

-- | Proof for transposing 'IncrementAll' and t'(!!)'.
incIdx
    :: SNat idx1 -> proxy1 idx2 -> SList proxy2 scope
    -> CmpNat idx1 (Length scope) :~: 'LT
    -> IncrementAll idx2 scope !! idx1 :~: Increment idx2 (scope !! idx1)
incIdx idx1 _ SNil lt = absurd $ zeroNoLT idx1 lt
incIdx idx1 idx2 (_ :^ scope) lt = case sCmpNat idx1 SZero of
    SLT -> absurd $ zeroNoLT idx1 Refl
    SEQ -> Refl
    SGT -> withProof (flipCmpNat idx1 SZero)
        $ withProof (ltRightPredSucc SZero idx1 Refl)
        $ withProof (incIdx (sPred idx1) idx2 scope lt) Refl
{-# RULES "Proof/incIdx" incIdx
    = \_ _ _ -> Unsafe.unsafeCoerce #-}
{-# INLINE [1] incIdx #-}

-- | Proof for transposing 'SubstituteAll' and t'(!!)'.
subIdx
    :: SNat idx1 -> proxy1 idx2 -> proxy2 tsub -> SList proxy3 scope
    -> CmpNat idx1 (Length scope) :~: 'LT
    -> SubstituteAll idx2 tsub scope !! idx1
        :~: Substitute idx2 tsub (scope !! idx1)
subIdx idx1 _ _ SNil lt = absurd $ zeroNoLT idx1 lt
subIdx idx1 idx2 tsub (_ :^ scope) lt = case sCmpNat idx1 SZero of
    SLT -> absurd $ zeroNoLT idx1 Refl
    SEQ -> Refl
    SGT -> withProof (flipCmpNat idx1 SZero)
        $ withProof (ltRightPredSucc SZero idx1 Refl)
        $ withProof (subIdx (sPred idx1) idx2 tsub scope lt) Refl
{-# RULES "Proof/subIdx" subIdx
    = \_ _ _ _ -> Unsafe.unsafeCoerce #-}
{-# INLINE [1] subIdx #-}

-- | Proof for transposing 'IncrementAll' and 'Length'.
incAllLen
    :: proxy1 idx -> SList proxy2 scope
    -> Length (IncrementAll idx scope) :~: Length scope
incAllLen _ SNil = Refl
incAllLen idx (_ :^ scope) = withProof (incAllLen idx scope) Refl
{-# RULES "Proof/incAllLen" incAllLen
    = \_ _ -> Unsafe.unsafeCoerce Refl #-}
{-# INLINE [1] incAllLen #-}

-- | Proof for transposing 'SubstituteAll' and 'Length'.
subAllLen
    :: proxy1 idx -> proxy2 tsub -> SList proxy3 scope
    -> Length (SubstituteAll idx tsub scope) :~: Length scope
subAllLen _ _ SNil = Refl
subAllLen idx tsub (_ :^ scope) = withProof (subAllLen idx tsub scope) Refl
{-# RULES "Proof/subAllLen" subAllLen
    = \_ _ _ -> Unsafe.unsafeCoerce Refl #-}
{-# INLINE [1] subAllLen #-}

-- | Proof that 'Increment' on a foreign type is a no-op.
incForeign
    :: proxy1 idx -> SList proxy2 ltargs -> proxy3 ltret
    -> BuildForeignType ltargs ltret
        :~: Increment idx (BuildForeignType ltargs ltret)
incForeign _ SNil _ = Refl
incForeign idx (_ :^ ltargs) ltret
    = withProof (incForeign idx ltargs ltret) Refl
{-# RULES "Proof/incForeign" incForeign
    = \_ _ _ -> Unsafe.unsafeCoerce Refl #-}
{-# INLINE [1] incForeign #-}

-- | Proof that 'Substitute' on a foreign type is a no-op.
subForeign
    :: proxy1 idx -> proxy2 tsub -> SList proxy3 ltargs -> proxy4 ltret
    -> BuildForeignType ltargs ltret
        :~: Substitute idx tsub (BuildForeignType ltargs ltret)
subForeign _ _ SNil _ = Refl
subForeign idx tsub (_ :^ ltargs) ltret
    = withProof (subForeign idx tsub ltargs ltret) Refl
{-# RULES "Proof/subForeign" subForeign
    = \_ _ _ _ -> Unsafe.unsafeCoerce Refl #-}
{-# INLINE [1] subForeign #-}

-- | Proof that 'ArgCount' is a left inverse of 'BitTuple'.
countBitTuple :: SNat size -> ArgCount (BitTuple size) :~: size
countBitTuple SZero = Refl
countBitTuple (SSucc size) = withProof (countBitTuple size) Refl
{-# RULES "Proof/countBitTuple" countBitTuple
    = \_ -> Unsafe.unsafeCoerce Refl #-}
{-# INLINE [1] countBitTuple #-}
