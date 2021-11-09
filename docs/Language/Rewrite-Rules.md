## Emit

Possible FFI-related input expressions:

* `#pureIO` from foreign primitives and foreign imports
* `#bindIO` from foreign primitives and foreign imports
* `#loadPointer` from foreign primitives
* `#storePointer` from foreign primitives
* `#call` from foreign imports
* `{#vector ...}` from foreign imports
* `#isolate{m}i{n}` from foreign exports
<!-- * `#testBit` from foreign exports -->
* `{#op _}` from foreign exports

### Phase A

Rules:

* Base rules
* `#bindIO @t (#bindIO @s x @t f)` to `(λs λ(s → IO t) Λ λ(t → IO 0) #bindIO @s 2 @0 (λs #bindIO @t (2 0) @0 1)) x f`
* `#bindIO @t (#pureIO @t x)` to `(λt Λ λ(t → IO 0) 0 1) x`
<!-- * `#bindIO @t x` to `(λt Λ λ(t → IO 0) #bindi{n} 1 @0 (λt {!marshallIn @t} 0 1)) x` if `t` is marshallable -->

After normalisation:

* `Expr` is
  * `λi{n} expr`
  * an `IOExpr1`
  where `expr` is an `Expr`.
* `IOExpr1` is
  * `#bindIO @s y @t (λs mx)`
  * `x @(IO t) mx my`
  * an `IOExpr2`
  where `x` is a `BaseExpr`, `y` is an `IOExpr3`, and `mx` and `my` are `IOExpr1`.
* `IOExpr2` is
  * `#pureIO @t x`
  * an `IOExpr3`
  where `x` is a `BaseExpr`.
* `IOExpr3` is
  * `#loadPointer op`
  * `#storePointer op x`
  * `#call {...}`
  * `{#vector ...}`
  * `#isolate{m}i{n} op`
  <!-- * `#testBit op` -->
  * `x @(IO t) mx my`
  where `x` is a `BaseExpr` and `mx` and `my` are `IOExpr1`.
* `OpExpr` is
  * `{#op _}`
  * `x @i{n} opx opy`
  where `x` is a `BaseExpr` and `opx` and `opy` are `OpExpr`.

### Phase B

Requirements:

* The context has the `IRBuilder` effect.

Rules:

* Phase A rules
* `λi{n} expr` to `(λi{n} expr) {#op i{n}}` via `IRBuilder`
* `#bindIO @s (#loadPointer {#op i{n}*}) @t f` to `f {#op i{n}}` via `IRBuilder`
* `#bindIO @(∀ 0 → 0) (#storePointer {#op i{n}*} x) @t f` to `f (Λ λ0 0)` via `IRBuilder`
* `#bindIO @s (#call {...}) @t f` to `f {#op i{n}}` via `IRBuilder`
* `#bindIO @s {#vector ...} @t f` to `f {#op i{n}}` via `IRBuilder`
* `#bindIO @(∀ 0 → 0 → 0) (#isolate{m}i{n} {#op i{n}}) @t f` to `f {#op i1}` via `IRBuilder` (complex)
<!-- * `#bindIO @(∀ 0 → 0 → 0) (#testBit {#op i1}) @t f` to `f {#op i{n}}` via `IRBuilder` (complex) -->
* `#pureIO @s x` to `{#op i{n}}` via `IRBuilder`

After normalisation, `Expr` is `{#op i{n}}`.

#### The Complex Rule

The rule to fold `#testBit` is complex as it may need to generate branch instructions.
We'd like to generate `select` instructions where possible as a full branch requires evaluating the bind RHS twice: once for `Λ λ0 λ0 0` and once for `Λ λ0 λ0 1`.
When we do have to branch, it'd be ideal to minimise the size of the branched RHS.

1. If the bind RHS is not a lambda, eta expand it.
2. For every reference `x` to the variable bound by the bind RHS lambda:
  1. Substitute every reference in the subexpressions of the branch with their known value. In other words, `x @t y z` to `x @t y[x/Λ λ0 λ0 1] z[x/Λ λ0 λ0 0]`.
  2. If it is not instantiated to `i{n}` or a marshallable `IO` type, apply these rules until it is:
    * `x @(a → b) f g y` to `x @b (f y) (g y)`
    * `f (x @a y z)` to `x @b (f y) (f z)`
    * `x @(∀ a) f g @b` to `x @(a[0/b]) (f @b) (g @b)`
    * `λa x @b y z` to `x @(a → b) (λa y) (λa z)`
    * `Λ x @a y z` to `x @(∀ a) (Λ y) (Λ z)`
  3. If it is instantiated to `i{n}`, then emit a `select` for the reference.
  4. If it is instantiated to `IO i{n}` and both branches are of the form `pureIO @i{n} _`, then emit a `select` for the reference.
  5. If it is instantiated to `IO t`, then emit a `br` for the reference.

Notes:

* Determining free variables is trivial if the RHS was first eta-expanded by step 1.
* Using a preordered search for step 2 may generate better LLVM, because step 2.1 may also eliminate variables in subexpressions.
* For step 2.2, minimising the size of the branch by hoisting the other way around would be better but it's also more complicated because it won't always give us a solution. Step 2.4 attempts to mitigate the downsides of the simpler approach; it might be possible to do something similar for `bindIO`.
* To avoid unnecessary branching when passing values between foreign imports, `marshallIn` and `marshallOut` primitives can be used with the rules `marshallIn @t (marshallOut @t x)` to `x` and `marshallOut @t (marshallIn @t x)`. Under `IO`, this can be done by storing both the original value and the marshalled value in a special-purpose constructor; rules which match on the constructor are then free to choose the value that works best.
