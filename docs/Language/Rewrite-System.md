*These notes reflect the decisions behind the design of the compiler.*
*They may not be an entirely accurate representation of the actual implementation.*

The rewrite system is a crucial part of the compiler, as both optimisation and code generation are built on top of it.
It therefore needs to be carefully thought out to maximise performance and minimise time spent debugging.
This document sketches out the design of the rewrite system.

First, let's start with how using the rewrite system would look:

1. Describe all the rewrite rules.
2. The rewrite system inspects the rules and works out relations between them.
   For example, if one rule outputs an `A`, then rules that apply to `A` can then take place.
   This allows us to skip checking other rules, since we already know they can't apply.
   With those relations, we can work out a directed graph of rules.

Then, to rewrite an expression:

1. Try all the rules until one can successfully be applied. If none can be applied, you're done.
2. Try rules that can apply after the last successful rule. If none can be applied, you're done.

Once we've determined that a particular rule can be applied, here's how we'd apply it:

1. Translate the rule's input into a lookup function capable of converting a key into the correct expression.
   This is fairly straightforward: walk through the input match and the expression simultaneously until the key matches.
   Once it matches, the corresponding subexpression is the result of the key.
   It may save time to use a map for this and populating the map at the same time as checking whether the rule can be applied.
2. Using the lookup function produced in step 1, translate the rule's output into an expression.

Looking through the existing rules, here are a few notes that may influence the design of the system.

* We need to be able to express the fact that two types must be related.
* Rules may output new `Var` expressions, but they will never match `Var` expressions.
* It is likely possible, though very impractical, to redefine every rule to never output `Var` expressions.
* Expressions that match wildcard patterns are sometimes modified by functions (for example to increment de Bruijn indices) before being output.
* Functions on expressions can sometimes require multiple expressions as input; it seems that the best way to do this is to allow a generic output which passes the lookup function to a user-defined function to let it look up whatever expressions it needs.

Since we need to apply the rules to the tree at each branch, and not just at the root, we also need to define how to handle that recursion, however it's unclear whether a breadth-first or depth-first strategy is simplest to implement.

Finally, for debugging purposes, we want to be able to hook into the rewrite system to print out every rewrite applied to the expression.

# Structure Metadata

To keep things simple, let's work with the following expression type (`Type` doesn't really matter here).

```haskell
data Expr
    = Var Int
    | App r r
    | TypeApp r Type
    | Lam Type r
    | TypeLam r
```

## Option 1

To describe what we know of the expression's structure, let's first write a functor whose fixed point is the expression type. Simplified, we have:

```haskell
data ExprF r
    = Var Int
    | App r r
    | TypeApp r Type
    | Lam Type r
    | TypeLam r
```

Using a fixed point combined with `Maybe`, we can effectively describe whether a structure is known or not:

```haskell
data Matching f = Any | Only (f (Matching f))
```

For example, the application substitution `(λ x) y ↦ x[y]` can only be applied to expressions matching `Only (App (Only (Lam _ Any)) Any)`.

It's unclear how to handle de Bruijn indices within the structure.
Ideally, we'd want to label them and encode cases where they must match, since that then allows us to transform the structure of the expression alongside the expression itself without having to reinspect things.
Let's add a label to the `Any` case (isomorphic to the fixed point version of `Either`).

```haskell
data Matching l f = Any l | Only (f (Matching l f))
```

There's no benefit to storing what we know about a given expression (we can just use the expression itself), however this fixed point is nonetheless useful for describing rewrite rules.
Aside from building a directed graph of the rule set, we can also build the functions that apply the rules.
This requires an understanding of the structure of the functor `f`.

```haskell
data Fix f = Fix (f (Fix f))

matches :: Fix ExprF -> Matching l ExprF -> Maybe (Map l ExprF)
```

We observe that `Matching l f` is isomorphic to `Fix (Compose (Either l) f)`.

```haskell
iso1 :: Functor f => Matching l f -> Fix (Compose (Either l) f)
iso1 matching = Fix $ Compose $ case matching of
    Any l -> Left l
    Only x -> Right $ iso1 <$> x

iso2 :: Functor f => Fix (Compose (Either l) f) -> Matching l f
iso2 (Fix f) = case getCompose f of
    Left l -> Any l
    Right x -> Only $ iso2 <$> x
```

So we don't need `Matching`.
Let's now try to find the perfect functor to compose with `ExprF` for rules.
For input, we need to be able to:

* Match a specific expression and all of its subexpressions.
* Match any expression and give it an index for later use.
* Match a specific type expression and all of its subexpressions.
* Match any type expression and give it a type index for later use.

For output, we need to be able to:

* Output a specific expression and all of its subexpressions.
* Output an expression generated from the index lookup function and the type index lookup function.
* Output a specific type expression and all of its subexpressions.
* Output any type expression generated from the type index lookup function.

In the simpler case of rewriting types (on which rewriting expressions seems to depend), `Either i` is sufficient for input and the following `TypeOut` profunctor is sufficient for output.

```haskell
data TypeF a
    = Arrow a a
    | Forall a
    | TypeVar Int

type TypeIn i = Either i
type TypeOut = (->)
```

Given that there's a profunctor on the output side, perhaps there's also a profunctor on the input side.

It looks like we need control over the type of the type in the expression tree: we want `Fix TypeF` in expressions and `Fix (Compose f TypeF)` in rules; let's turn `ExprF` into a bifunctor.

```haskell
data ExprF t r
    = Var Int
    | App r r
    | TypeApp r t
    | Lam t r
    | TypeLam r

type ExprIn i = Either i
data ExprOut tlookup elookup a = ExprOut (tlookup -> elookup -> a)
```

In order to check whether an expression matches a rule input, we can first fold the rule input into a `Kleisli Maybe`.
For example, for the simpler type expressions, we'd want to fold rules into `Fix TypeF -> Maybe tlookup`.
Before we can do that, we need to settle on a type for `tlookup` and `elookup`.
We could try and build a safe lookup type, but to benefit from it, we'd need to encode the used indices into the rule type.
Instead, we can settle for using `IntMap` and writing tests to check that lookups work (most of the time).

Now let's try to convert a rule input into a matching function.

```haskell
type TypeRuleIn = Fix (Compose (TypeIn Int) TypeF)
type ExprRuleIn = Fix (Compose (ExprIn Int) (ExprF TypeRuleIn))

type TypeLookup = IntMap (Fix TypeF)
type ExprLookup = IntMap (Fix ExprF)

cata :: forall f a. Functor f => (f a -> a) -> Fix f -> a
cata f = go
  where
    go :: Fix f -> a
    go (Fix x) = f $ go <$> x

match :: TypeRuleIn -> Fix TypeF -> Maybe TypeLookup
match = cata $ \cta tb -> case getCompose cta of
    Left idx -> Just $ IM.singleton idx tb
    Right ta -> case (ta, tb) of
        (Arrow tax tay, Arrow tbx tby) -> liftA2 (<>) (tax tbx) (tay tby)
        (Forall tax, Forall tbx) -> tax tbx
        (TypeVar ix, TypeVar iy) -> if ix == iy then Just mempty else Nothing
        (_, _) -> Nothing

bimatch :: ExprRuleIn -> Fix ExprF -> Maybe (ExprLookup, TypeLookup)
bimatch = cata $ \cea eb -> case getCompose cea of
    Left idx -> Just (IM.singleton idx eb, IM.empty)
    Right ea -> case (ea, eb) of
        (Var ix, Var iy) -> if ix == iy then Just mempty else Nothing
        (App eax eay, App ebx eby) -> liftA2 (<>) (eax ebx) (eay eby)
        (TypeApp eax tay, TypeApp ebx tby) -> liftA2 (<>) (eax ebx) ((,) IM.empty <$> match tay tby)
        (Lam tax eay, Lam tbx eby) -> liftA2 (<>) ((,) IM.empty <$> match tax tbx) (eay eby)
        (TypeLam eax, TypeLam ebx) -> eax ebx
        (_, _) -> Nothing
```

Once we've found a match, we can produce its corresponding output.

```haskell
type TypeRuleOut = Fix (Compose (TypeOut TypeLookup) TypeF)
type ExprRuleOut = Fix (Compose (ExprOut TypeLookup ExprLookup) (ExprF TypeRuleOut))

rewriteMatch :: TypeRuleOut -> TypeLookup -> Fix TypeF
rewriteMatch = cata $ \cta -> Fix . getCompose cta

rewriteBimatch :: TypeLookup -> ExprLookup -> ExprRuleOut -> Fix ExprF
rewriteBimatch tlookup elookup = cata $ \cea -> Fix $ case getCompose cea of
    ExprOut f -> f tlookup elookup
```

The simplest way of applying rewrite rules would be a depth-first strategy.
However, the current rules for floating `Emit` out of expressions require the use of a breadth-first strategy to avoid running IO actions without regard for whether or when the action is output by the function.
There are several solutions:

1. Change the `Emit` floating rules such that they can't float overaggressively.
   This doesn't seem viable due to how it would likely require inspecting the superexpression, something that is bound to result in hard-to-find bugs.
2. Split the rewrite into two phases: the first with all the branching rules and the second with all the `Emit` rules.
   This doesn't seem viable due to how a runtime branch can determine whether an action should run or not; it can't be rewritten in the first phase because it needs `Emit`, but it also can't be rewritten in the second phase because the problem would persist.
3. Float `Emit` separately.
   This doesn't seem viable due to how `Emit` not only gets in the way when trying to match a rule, but it also prevents us from even inspecting its subexpression.
4. Rethink `Emit` to avoid needing overaggressive rewrite rules in the first place.
   This could be viable if use of `Emit` (or its equivalent replacement) could be restricted to expressions of a particular type.
5. Give up and use a breadth-first strategy.

### Depth-First

The goal of this section is to explore how `Emit` could be replaced to enable depth-first expression rewriting.

`Emit` is only needed in rules which match an `LlvmOperand`.
On the output side, `Emit`'s subexpression is always either an `LlvmOperand` or a `Unit`.
This suggests that its power is only needed when handling `LlvmOperand` and `Unit`.
These constructors correspond to expressions of type `i{n}` and `IO (∀ 0 -> 0)` respectively.
Given how easy it is to get confused between `i{n}` and `IO` types, it may be best to make `i{n}` a synonym of the other.
`Emit` could then be replaced with an `IOValue`, which would store the LLVM operand (or nothing for `Unit`) under the LLVM builder monad.
This presents a number of benefits:

* Reasoning about where certain code is generated is easier, since codegen should follow `IO` semantics.
* Unlike `Emit`, `IOValue` doesn't need to be explicitly floated, because rules that reduce such values will take care of it.
* Since we don't need rules to float `IOValue` out, we should now have a confluent rewrite system and we no longer need to rewrite breadth-first.
* Prettyprinting no longer needs to hack into `Emit`; it can just render `IOValue` as `{op}` or `()`.

Finally, to perform the rewrite, we simply need to apply a `rewrite` catamorphism that repeatedly tries to apply every rule to the expression.

### Threading a Rewriter Monad

To enable easy debugging of rewrite rules, we also want to thread a rewriter monad through the entire computation.
This can be achieved in two straightforward changes:

* Add the monad to the output of `rewrite` and adjust definitions appropriately.
* Add the monad to `TypeOut` and `ExprOut` and adjust definitions accordingly.

Calls to special monad operations can then be added to allow the user to track the progress of the computation.
For example, the user can write progress to a log file and then see what rules are being applied in the file.

### Performance

When the rules are any more complex than a simple top-level match, the implementation described above has to force subexpressions to check whether the current expression matches.
With a depth-first strategy, this causes the entire expression tree (or most of it) to be forced before a single rule is applied, leading to a memory leak which is superlinear in the expression size.
Using a strict expression functor does resolve the leak, giving us memory usage which is linear in the expression size, however it slightly lengthens run time.
More specifically, a strict `Functor` instance seems to have almost identical memory usage benefits.

Performance is best when using the `Fix` datatype from `data-fix` (`Mu` is bad and `Nu` is abysmal).
On a toy Peano arithmetic rewrite system, it allowed over 1M rewrites/second and linear memory usage without needing a strict functor.
In fact, increasing strictness made performance *worse*.
Conveniently, `Fix` is also the easiest of the three to construct intuitively.
Threading a monad through the rewrite does not completely negate the performance benefits.

## Option 2

Let's try a more direct approach: encode structure as a tree.

```haskell
data RecTree = Leaf | Branch [RecTree]
```

We can then add some boilerplate to project an expression into its structure.

```haskell
proj :: Expr -> RecTree
proj expr = case expr of
    Var _ -> Leaf
    App f x -> Branch [proj f, proj x]
    TypeApp f _ -> Branch [proj f]
    Lam _ x -> Branch [proj x]
    TypeLam x -> Branch [proj x]
```

This removes a bit too much information, so let's try adding a label.

```haskell
data LabelType = LeafLabel | BranchLabel

data RecTree l = Leaf (l LeafLabel) | Branch (l BranchLabel) [RecTree l]
```

Now we can label each constructor.

```haskell
data ExprL lt where
    VarL :: ExprL 'LeafLabel
    AppL :: ExprL 'BranchLabel
    TypeAppL :: ExprL 'BranchLabel
    LamL :: ExprL 'BranchLabel
    TypeLamL :: ExprL 'BranchLabel
```

For better type safety, it may be wise to replace `[RecTree l]` with `Vector n (RecTree l)` and include `n` in the label tag.
With that done, we notice that `Leaf (l LeafLabel)` is the same as `Branch (l (BranchLabel 0))`; it's better to avoid any possibility of confusion by removing `Leaf` (requiring `n` > 0 is messier).

```haskell
data RecTree l where
    RecTree :: n => l n -> Vector n (RecTree l) -> RecTree l

data ExprL (lt :: Nat) where
    VarL :: ExprL 0
    AppL :: ExprL 2
    TypeAppL :: ExprL 1
    LamL :: ExprL 1
    TypeLamL :: ExprL 1
```

To avoid losing information when applying rules, we also need some way of naming each `RecTree` so that we can reference it later.
We'll add optional numbers to the labels for this.
We also need some way of encoding when a rule should match any expression, so let's add a special label for that.

```haskell
-- Short for Indexed Label
type IL :: (Nat -> Type) -> Nat -> Type
data IL l n = IL (Maybe Int) (l n)

type MaybeL :: (Nat -> Type) -> Nat -> Type
data MaybeL (l :: Nat -> Type) (n :: Nat) where
    -- | Match anything
    NothingL :: MaybeL l 0
    -- | Match only the given label
    JustL :: l n -> MaybeL l n
```

Let's now look at how we might go about defining our rules.

```haskell
data Rule l = (:->) (RecTree l) (RecTree l)

appRule :: Rule (IL (MaybeL ExprL))
appRule = RecTree (IL Nothing $ JustL AppL)
    (  RecTree (IL Nothing $ JustL LamL)
        (  RecTree (IL (Just 0) NothingL)
        :> VNil
        )
    :> RecTree (IL (Just 1) NothingL)
    :> VNil
    ) :-> RecTree (IL Nothing $ )
```
