{-# LANGUAGE PatternSynonyms #-}

-- | Synonyms for more concise AST-related code.
module Language.Elemental.Syntax.Synonyms
    (module Language.Elemental.Syntax.Synonyms) where

import Data.Fix (Fix(Fix))

import Language.Elemental.Rewrite
import Language.Elemental.Syntax.Internal


-- * Unannotated Expressions

-- | 'App' synonym for unannotated expressions.
pattern (:$:) :: Expr -> Expr -> Expr
pattern ef :$: ex = Fix (App ef ex)
infixl 2 :$:

-- | 'TypeApp' synonym for unannotated expressions.
pattern (:@:) :: Expr -> Type -> Expr
pattern ef :@: tx = Fix (TypeApp ef tx)
infixl 2 :@:

-- | 'Lam' synonym for unannotated expressions.
pattern (:\:) :: Type -> Expr -> Expr
pattern tx :\: ey = Fix (Lam tx ey)
infixr 1 :\:

-- | v'Arrow' synonym for unannotated types.
pattern (:->:) :: Type -> Type -> Type
pattern tx :->: ty = Fix (Arrow tx ty)
infixr 3 :->:

-- | 'Var' synonym for unannotated expressions.
pattern V :: Int -> Expr
pattern V idx = Fix (Var idx)

-- | 'TypeVar' synonym for unannotated types.
pattern TV :: Int -> Type
pattern TV idx = Fix (TypeVar idx)

-- | 'TypeLam' synonym for unannotated expressions.
pattern TL :: Expr -> Expr
pattern TL ex = Fix (TypeLam ex)

-- | 'Forall' synonym for unannotated types.
pattern FA :: Type -> Type
pattern FA tx = Fix (Forall tx)

-- | 'IOType' synonym for unannotated types.
pattern IOT :: Type -> Type
pattern IOT tx = Fix (SpecialType (IOType tx))

-- | 'PointerType' synonym for unannotated types.
pattern PtrT :: PointerKind -> Type -> Type
pattern PtrT pk tx = Fix (SpecialType (PointerType pk tx))

-- | v'InternalExpr' synonym for unannotated expressions.
pattern IE :: InternalExpr Type Expr -> Expr
pattern IE ie = Fix (InternalExpr ie)

-- | v'InternalType' synonym for unannotated types.
pattern IT :: InternalType Type -> Type
pattern IT it = Fix (SpecialType (InternalType it))

-- * Rule Input Expressions

-- | Type synonym for expression rule inputs.
type ExprRuleIn a = BiruleIn ExprF TypeF a

-- | Type synonym for type rule inputs.
type TypeRuleIn a = RuleIn TypeF a

-- | 'App' synonym for rule inputs.
pattern (:$?) :: ExprRuleIn a -> ExprRuleIn a -> ExprRuleIn a
pattern ef :$? ex = Fix (Bimatch (App ef ex))
infixl 2 :$?

-- | 'TypeApp' synonym for rule inputs.
pattern (:@?) :: ExprRuleIn a -> TypeRuleIn a -> ExprRuleIn a
pattern ef :@? tx = Fix (Bimatch (TypeApp ef tx))
infixl 2 :@?

-- | 'Lam' synonym for rule inputs.
pattern (:\?) :: TypeRuleIn a -> ExprRuleIn a -> ExprRuleIn a
pattern tx :\? ey = Fix (Bimatch (Lam tx ey))
infixr 1 :\?

-- | v'Arrow' synonym for rule inputs.
pattern (:->?) :: TypeRuleIn a -> TypeRuleIn a -> TypeRuleIn a
pattern tx :->? ty = Fix (Match (Arrow tx ty))
infixr 3 :->?

-- | 'Var' synonym for rule inputs.
pattern VI :: Int -> ExprRuleIn a
pattern VI idx = Fix (Bimatch (Var idx))

-- | 'TypeVar' synonym for rule inputs.
pattern TVI :: Int -> TypeRuleIn a
pattern TVI idx = Fix (Match (TypeVar idx))

-- | 'TypeLam' synonym for rule inputs.
pattern TLI :: ExprRuleIn a -> ExprRuleIn a
pattern TLI ey = Fix (Bimatch (TypeLam ey))

-- | 'Forall' synonym for rule inputs.
pattern FAI :: TypeRuleIn a -> TypeRuleIn a
pattern FAI ty = Fix (Match (Forall ty))

-- | 'IOType' synonym for rule inputs.
pattern IOTI :: TypeRuleIn a -> TypeRuleIn a
pattern IOTI tx = Fix (Match (SpecialType (IOType tx)))

-- | 'PointerType' synonym for rule inputs.
pattern PtrTI :: PointerKind -> TypeRuleIn a -> TypeRuleIn a
pattern PtrTI pk tx = Fix (Match (SpecialType (PointerType pk tx)))

-- | v'InternalExpr' synonym for rule inputs.
pattern IEI :: InternalExpr (TypeRuleIn a) (ExprRuleIn a) -> ExprRuleIn a
pattern IEI ie = Fix (Bimatch (InternalExpr ie))

-- | v'InternalType' synonym for rule inputs.
pattern ITI :: InternalType (TypeRuleIn a) -> TypeRuleIn a
pattern ITI it = Fix (Match (SpecialType (InternalType it)))

-- * Rule Output Expressions

-- | Type synonym for expression rule outputs.
type ExprRuleOut a = BiruleOut ExprF TypeF a

-- | Type synonym for expression rule outputs.
type TypeRuleOut a = RuleOut TypeF a

-- | 'App' synonym for rule outputs.
pattern (:$=) :: ExprRuleOut a -> ExprRuleOut a -> ExprRuleOut a
pattern ef :$= ex = Fix (Bistatic (App ef ex))
infixl 2 :$=

-- | 'TypeApp' synonym for rule outputs.
pattern (:@=) :: ExprRuleOut a -> TypeRuleOut a -> ExprRuleOut a
pattern ef :@= tx = Fix (Bistatic (TypeApp ef tx))
infixl 2 :@=

-- | 'Lam' synonym for rule outputs.
pattern (:\=) :: TypeRuleOut a -> ExprRuleOut a -> ExprRuleOut a
pattern tx :\= ey = Fix (Bistatic (Lam tx ey))
infixr 1 :\=

-- | v'Arrow' synonym for rule outputs.
pattern (:->=) :: TypeRuleOut a -> TypeRuleOut a -> TypeRuleOut a
pattern tx :->= ty = Fix (Static (Arrow tx ty))
infixr 3 :->=

-- | 'Var' synonym for rule outputs.
pattern VO :: Int -> ExprRuleOut a
pattern VO idx = Fix (Bistatic (Var idx))

-- | 'TypeVar' synonym for rule outputs.
pattern TVO :: Int -> TypeRuleOut a
pattern TVO idx = Fix (Static (TypeVar idx))

-- | 'TypeLam' synonym for rule outputs.
pattern TLO :: ExprRuleOut a -> ExprRuleOut a
pattern TLO ey = Fix (Bistatic (TypeLam ey))

-- | 'Forall' synonym for rule outputs.
pattern FAO :: TypeRuleOut a -> TypeRuleOut a
pattern FAO ty = Fix (Static (Forall ty))

-- | 'IOType' synonym for rule outputs.
pattern IOTO :: TypeRuleOut a -> TypeRuleOut a
pattern IOTO tx = Fix (Static (SpecialType (IOType tx)))

-- | 'PointerType' synonym for rule outputs.
pattern PtrTO :: PointerKind -> TypeRuleOut a -> TypeRuleOut a
pattern PtrTO pk tx = Fix (Static (SpecialType (PointerType pk tx)))

-- | v'InternalExpr' synonym for rule outputs.
pattern IEO :: InternalExpr (TypeRuleOut a) (ExprRuleOut a) -> ExprRuleOut a
pattern IEO ie = Fix (Bistatic (InternalExpr ie))

-- | v'InternalType' synonym for rule outputs.
pattern ITO :: InternalType (TypeRuleOut a) -> TypeRuleOut a
pattern ITO it = Fix (Static (SpecialType (InternalType it)))
