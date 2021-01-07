This document breaks down the design decisions behind the Elemental language and how a compiler might implement the design.

The aim is to design and implement a compiled total functional programming language.
Here, "total" means that there is no possibility of non-termination; every valid program must always terminate in finite time.

We start from System F, which is strongly normalising and therefore total.
For reasons that will become clear later, we extend the language to support existential types.
Doing so does not increase the number of functions we can implement, since `∃x. σ` is equivalent to `∀r. (∀x. σ -> r) -> r` (assuming `r` isn't free in `σ`), but it does make the type expressions in the rest of this document clearer.

Most other functional languages extend the simply typed lambda calculus with a fixed point combinator of type `∀a. (a → a) → a` to enable the implementation of general recursive functions.
The class of general recursive functions is Turing complete, so the halting problem is undecidable for those languages.
Since fixed point combinators lead to Turing completeness, we need a different mechanism for recursion.

# Recursion Schemes

In [1], Meijer et al. present a way of representing recursive data with fixed points of functors.
They then go on to use those recursive data types to define recursion schemes over arbitrary data types.
In [2], Wadler presented a non-recursive construction for both the least fixed point and the greatest fixed point of any functor.
By combining these two pieces of work, we can compute certain recursive functions.

The least fixed point for a functor `F` is given by `μ F = ∀X. (F X → X) → X`, whereas the greatest fixed point is given by `ν F = ∃X. (X → F X) × X`.
In an Haskell-like syntax, these can be written as follows.

```haskell
type Mu f = forall x. (f x -> x) -> x
type Nu f = forall r. exists x. ((x -> f x) -> x -> r) -> r
```

The `Mu f` type gives rise to the catamorphism function `cata :: forall a. (f a -> a) -> Mu f -> a`, which folds recursive data into an `a`.
The `Nu f` type gives rise to the anamorphism function `ana :: forall a. (a -> f a) -> a -> Nu f`, which unfolds an `a` into corecursive codata.

```haskell
cata :: forall a. (f a -> a) -> Mu f -> a
cata f x = x f

ana :: forall a. (a -> f a) -> a -> Nu f
ana f x g = g f x
```

These functions can then be used to implement certain recursive functions.

```haskell
type ListF a b = forall r. r -> (a -> b -> r) -> r

sum :: Mu (ListF Integer) -> Integer
sum = cata (\f -> f 0 (+))
-- With recursion: sum (x : xs) = x + sum xs

naturals :: Nu (ListF Integer)
naturals = ana (\n nil cons -> cons n (n + 1)) 0
-- With recursion: naturals = 0 : map (+1) naturals
```

So what class of functions can be implemented with this?

First, let's take a look at primitive recursive functions.
Four of the five axioms for primitive recursive functions trivially hold without recursion.
The non-trivial axiom states that given a k-ary primitive recursive function `f` and a (k+2)-ary primitive recursive function `g`, a function `h` is primitive recursive if

```
h(0, x1, x2, ..., xk) = f(x1, x2, ..., xk)
h(S(y), x1, x2, ..., xk) = g(y, h(y, x1, x2, ..., xk), x1, x2, ..., xk)
```

Rewriting in lambda calculus, where `x1`, `x2`, ..., `xk` are free variables, `h` is primitive recursive if

```
h 0 = f
h (S y) = g y (h y)
```

This corresponds to a catamorphism over the `Maybe` functor (note that `Nothing` is 0 and `Just` is the successor function).

```haskell
type Maybe a = forall r. r -> (a -> r) -> r

nothing :: forall a. Maybe a
nothing f _ = f

just :: forall a. a -> Maybe a
just x _ f = f x

zero :: Mu Maybe
zero f = f nothing

succ :: Mu Maybe -> Mu Maybe
succ n f = f (just (n f))

h = fst (cata (\x -> x (zero, f) (\(n, r) -> (succ n, g n r))))
  where
    fst (x, _) = x
```

Therefore all primitive recursive functions can be implemented using catamorphisms.

However, recursion schemes aren't limited to primitive recursive functions, since the Ackermann function `ack`, a well-known example of a non primitive recursive function, can also be implemented [3].

```haskell
ack :: Mu Maybe -> Mu Maybe -> Mu Maybe
ack = cata (\x -> x succ (cata (\y -> y (f (succ zero)) f)))
```

The Girard-Reynolds isomorphism [4] states that the class of functions that can be implemented in this system is isomorphic to second-order intuitionistic predicate logic.
Hopefully, that makes it expressive enough to implement a large number of useful programs.

# Nominal Types

We extend System F with a `newtype` syntax similar to Haskell that allows the user to define types that are not considered equivalent with the same representation.
This is to help the user avoid mixing together two representationally equivalent types with different intended semantics, without significantly increasing the complexity of the compiler or affecting its ability to identify similar representations.

```haskell
newtype Maybe a = Maybe { unMaybe :: forall r. r -> (a -> r) -> r }
```

The above definition generates two functions which may be used to convert to and from the nominal type.

* `Maybe :: forall a. (forall r. r -> (a -> r) -> r) -> Maybe a`
* `unMaybe :: forall a. Maybe a -> forall r. r -> (a -> r) -> r`

A `newtype` should be used when the type is interpreted with semantic meaning attached to it.
It should not be used if a function interprets the type in a purely representational way.

For example, 32-bit integers and IEEE-754 single-precision floating points have the same representation, but different semantics.
It would not make sense to perform floating point addition on two integers or on an integer and a floating point.
However, a function that writes 4 bytes of raw data into a file should not use a `newtype`, since it does not care about the semantic meaning of those 4 bytes.

Outside of the type checker, the compiler should treat nominal types the same as their underlying representation, since the name has no bearing on how the program will need to handle data; it is solely to prevent the user from confusing representationally equal types.

# Comparing Expressions

Comparing expressions is a decidable but non-trivial exercise.
Consider the following three possible definitions of `not`.

```haskell
type Bool = forall r. r -> r -> r

not1, not2, not3 :: Bool -> Bool
not1 = λ(b :: Bool). Λr. λ(x :: r). λ(y :: r). b @r y x
not2 = λ(b :: Bool). Λr. b @(r -> r -> r) (λ(_ :: r). λ(x :: r). x) (λ(_ :: r). λ(x :: r). x)
not3 = λ(b :: Bool). b @Bool (Λr. λ(_ :: r). λ(x :: r). x) (λ(_ :: r). λ(x :: r). x)
```

Firstly, the expressions must be rewritten with de Bruijn indices [6].
This allows us to check for α-equivalence (equal except that bound variables are named differently) for free.

```haskell
not1 = λBool Λ λ0 λ0 2 @0 0 1
not2 = λBool Λ 0 @(0 -> 0 -> 0) (λ0 λ0 0) (λ0 λ0 1)
not3 = λBool 0 @Bool (Λ λ0 λ0 0) (Λ λ0 λ0 1)
```

Next, we compare the expressions for equality, simplifying them to their normal form first if necessary.
In this example, all expressions are already in their normal form, so they are evidently not equal.

However, this does not exclude the possibility that they are extensionally equivalent, i.e. given equal inputs, they will produce the same outputs.
For brevity, we will refer to this as simply "equivalence".
To test this property, we compare the result of applying the functions instead of the functions themselves by introducing an unknown parameter.

```haskell
not1 a = (λBool Λ λ0 λ0 2 @0 0 1) a
       = Λ λ0 λ0 a @0 0 1

not2 a = (λBool Λ 0 @(0 -> 0 -> 0) (λ0 λ0 0) (λ0 λ0 1)) a
       = Λ a @(0 -> 0 -> 0) (λ0 λ0 0) (λ0 λ0 1)

not3 a = (λBool 0 @Bool (Λ λ0 λ0 0) (Λ λ0 λ0 1)) a
       = a @Bool (Λ λ0 λ0 0) (Λ λ0 λ0 1)
```

We can continue introducing unknown parameters until the two functions are either provably equivalent or not equivalent.
To further simplify these expressions, we will need the free theorem [7] for `Bool`: for all `a : Bool`:

1. for all `f : α -> β`, `x : α`, and `y : α`, we have `f (a @α x y) = a @β (f x) (f y)`
2. for all `f : α -> β`, `g : α -> β`, and `x : α`, we have `a @(α -> β) f g x = a @β (f x) (g x)`
3. for all `y : β` and `z : β`, we have `λx : α. a @β y z = a @(α -> β) (λx : α. y) (λx : α. z)`
4. for all `x : σ` and `y : σ`, we have `Λα. a @σ x y = a @(forall α. σ) (Λα. x) (Λα. y)`
5. for all `x : forall α. σ` and `y : forall α. σ`, we have `a @(forall α. σ) x y @α = a @σ (x @α) (y @α)`

(Note: some versions of the theorem are equipotent.)

By the 4th version of the free theorem, `not2` and `not3` are equivalent.

```haskell
not1 a @α = (Λ λ0 λ0 a @0 0 1) @α
          = λα λα a @α 0 1

not2 a @α = (Λ a @(0 -> 0 -> 0) (λ0 λ0 0) (λ0 λ0 1)) @α
          = a @(α -> α -> α) (λα λα 0) (λα λα 1)
```

To simplify `not2` further, we apply the 2nd version of the free theorem.

```haskell
not1 a @α b = (λα λα a @α 0 1) b
            = λα a @α 0 b

not2 a @α b = a @(α -> α -> α) (λα λα 0) (λα λα 1) b
            = a @(α -> α) ((λα λα 0) b) ((λα λα 1) b)
            = a @(α -> α) (λα 0) ((λα λα 1) b)
            = a @(α -> α) (λα 0) (λα b)

not1 a @α b c = (λα a @α 0 b) c
              = a @α c b

not2 a @α b c = a @(α -> α) (λα 0) (λα b) c
              = a @α ((λα 0) c) ((λα b) c)
              = a @α c ((λα b) c)
              = a @α c b
```

Therefore `not1`, `not2`, and `not3` are equivalent.

Introducing new variables for these equivalence proofs is impractical.
Luckily, it's not necessary, since we can simply use different versions of the free theorem instead.
Let's now consider the following expressions [8].

```haskell
type Unit = forall r. r -> r

unit1, unit2 :: Unit -> Unit
unit1 = λ(f :: Unit). Λα. λ(x :: α). f @Unit (Λβ. λ(y :: β). y) @α x
unit2 = λ(f :: Unit). Λα. λ(x :: α). f @α x
```

Again, we convert variables to de Bruijn indices and normalise the expressions.

```haskell
unit1 = λUnit Λ λ0 1 @Unit (Λ λ0 0) @0 0
      = λUnit Λ 0 @Unit (Λ λ0 0) @0
      = λUnit 0 @Unit (Λ λ0 0)

unit2 = λUnit Λ λ0 1 @0 0
      = λUnit Λ 0 @0
      = λUnit 0
```

The free theorem for `Unit` states that for all types `α` and `β` and for all `id : Unit`, `f : α -> β`, `x : ∀ β`, `y : α`, we have:

1. `id @(α -> β) f y = id @β (f y)`
2. `id @(∀ β) x @α = id @β (x @α)`

We can use these two versions of the free theorem to show that `unit1` and `unit2` are extensionally equivalent.
Note that where we don't have a `y` or an `@α` available, then we can use η-expansion to introduce them, as can be seen in the following corollary.

1. `id @(α -> β) f = λα id @β (f 0)`
2. `id @(∀ β) x @α = Λ id @β (x @0)`

```haskell
unit1 = λUnit 0 @Unit (Λ λ0 0)
      = λUnit Λ 0 @(0 -> 0) ((Λ λ0 0) @0)
      = λUnit Λ 0 @(0 -> 0) (λ0 0)
      = λUnit Λ λ0 1 @0 0

unit2 = λUnit Λ λ0 1 @0 0
```

Although `unit1` and `unit2` are not β,η-equivalent, we can still simplify them to the same expression using the free theorem.

If we extend the System F term rewrite system with rules that apply free theorems, then the equality of normal forms can better approximate extensional equivalence.
Using de Bruijn indices, the rewrite system would thus have the following rules.

* β-reduction: `(λα x) y ↦ x[y]` and `(Λ x) @α ↦ x[α]`
* η-reduction: `λα x 0 ↦ x` if `0` is not a free term in `x` and `Λ x @0 ↦ x` if `0` is not a free type in `x`
* [insert name here]: "distribute" an application into a polymorphic function's arguments using the free theorem for the function if any of the arguments "request" an additional application for further reduction.

These rewrite rules may be used to simplify expressions, simplifying them and improving the performance of the compiled program.
System F is strongly normalising, meaning that its rewrite rules will eventually lead to the same normal form, at which point nothing can be rewritten.
Thus, there is no risk of non-termination when simplifying.

It is unclear whether our extension of System F's rules is also strongly normalising.
Intuitively, it is at the very least weakly normalising, meaning that rewriting will eventually reach a normal form.
A rigorous proof is needed.

# The Foreign Function Interface

Having a pure functional calculus in which to perform computations is nice, but ultimately useless if we can't then do anything with the results of computations.
A Foreign Function Interface (FFI) is needed to interact with the outside world, though it needs to be designed carefully to avoid violating the soundness of the language.
The language is sound under the following assumptions.

1. All values are immutable.
2. Parametricity holds for all values (i.e. values don't violate the free theorem for their type).
3. Terms are referentially transparent (i.e. any term can be replaced with its expression without altering the behaviour of the program).
4. References to terms are not leaked past their lifetime.

Due to assumptions 1 and 2, we need some way of separating pure values from impure values produced in a foreign language.
This can be done by wrapping impure values in an opaque `IO` type [9].
An `IO a` value can be thought of as a description of an action that outputs an `a` value.
These `IO a` actions are immutable even though the action can output different values because the actions undertaken to get that output value are always the same.

So given an `IO a` action, how can we get back an `a`?
We can't; getting the output of an action from a description of that action doesn't make sense unless if we perform the action, but that would violate our assumptions, because the output of an action isn't immutable.
Instead, we compose `IO` actions together to describe more complex actions.
We add the primitive functions `return : forall a. a -> IO a` and `bind : forall a b. IO a -> (a -> IO b) -> IO b` to allow users to compose `IO` actions together.

## Importing and Exporting Functions

There are two possible interactions with foreign functions: calling foreign functions and being called by foreign functions.
In order to call a foreign function, we need to import it into the language.
For this to be sound, we need to know a few things about the function.

* the symbol name
* the calling convention
* the argument count and the types of arguments
* the return type

To be called by foreign functions, we need to export the function.
When exporting a function, we similarly need to know under what symbol name and calling convention to export the function; the type of the function is already apparent in its definition (though including the type is a good sanity check).

## Marshalling Types

At the foreign boundary, we need to ensure that data is in the right format, since the foreign language might use a different representation for the same type of data.
For example, in C, a `bool` is an integer (of a platform-dependant size) and somehow needs to be converted into a value of type `forall a. a -> a -> a`.

For types of a statically-known size, this can be achieved by composing built-in types that have a hardwired data representation.
Types with platform-dependant sizes are much more painful, as they would require some form of conditional compilation (or some way to avoid hard-coding the size).
Besides pointers, all such types can be handled by a shim written in a foreign language, so supporting platform-dependant sizes isn't a major concern.

LLVM's integer type `iN` can be encoded as a product of `N` bits to represent each bit of the integer type.
Individual bits simply correspond to `forall a. a -> a -> a`.
For example, `i8` is encoded as follows.

```haskell
type Bit = forall a. a -> a -> a

newtype I8 = I8 { unI8 :: forall r. (Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> r) -> r }
```

Although the above expression may be tedious to write correctly, the user only needs to write it once for each bit width they wish to use.

In the previous example, we defined `I8` as a nominal type and not a type alias.
Nominal types have the same representation as the underlying type, so the compiler simply ignores the name of the type when a nominal type is used in a foreign import or export declaration.

In conclusion, we have the following rules for valid types in a foreign import or export declaration:

* a product of `N` `Bit` for `iN`
* a pointer of a type with representation `X` for `X*`
* a product of types with representations `X1, X2, ..., Xk` for `{X1, X2, ..., Xk}` (ABI struct or packed struct?)

## Pointers

Support for pointers can be implemented using an opaque `ForeignPtr a` type and some primitive functions for manipulating pointers.

```haskell
loadPtr :: forall a. ForeignPtr a -> IO a
storePtr :: forall a. a -> ForeignPtr a -> IO a
```

`loadPtr` loads the value at the pointer and `storePtr` stores the given value into the pointer.
To simplify the implementation of the language, pointers must be allocated in foreign code.

That said, there is utility in being able to define pointers to hardcoded locations.
For example, on microprocessors with memory-mapped IO, it's often necessary to write to specific addresses.
Special syntax can be added to support these pointers, which must specify:

* a fixed address
* whether the address is read/write, read-only, or write-only
* whether reading the address has side effects (writing always has a side effect, since it changes memory)

Making best use of the information within the compiler requires using a different type for fixed pointers.
We want to define a single type to avoid having to define the primitives for each type.
We use phantom type parameters to encode the memory protection and the side effects: `Ptr rd wr re a`

* `rd` must be either `Readable` or `NoRead`
* `wr` must be either `Writable` or `NoWrite`
* `re` must be either `HasReadSE` or `NoReadSE`
* `a` is the type referenced by the pointer

Little compiler magic is required for `Ptr` if special kinds are used for the phantom parameters; only the kinds and their inhabitants need to be wired-in.
The fixed pointer syntax would likely end up looking somewhat as follows.

```haskell
foreign pointer 0xB8000 colourMonitor :: Ptr Readable Writable NoReadSE Monitor
```

With these phantom parameters, we then only need one version of each primitive:

```haskell
loadPtr :: forall wr re a. Ptr Readable wr re a -> IO a
storePtr :: forall rd re a. a -> Ptr rd Writable re a -> IO a

pureLoadPtr :: forall wr a. Ptr Readable wr NoReadSE a -> a
```

We can define `ForeignPtr` from `Ptr` to reduce the number of primitives and types that need to be wired in.

```haskell
type ForeignPtr rd wr a = Ptr rd wr HasReadSE a
```

Now, it makes sense to define a function to allocate a pointer.

```haskell
newPtr :: forall rd a r. a -> (forall re. Ptr rd NoWrite re a -> r) -> r
newWritablePtr :: forall rd a r. a -> (forall re. Ptr rd Writable re a -> r) -> r
```

This definition uses the `re` phantom parameter to prevent the pointer from being leaked out the result [10].
Although this still doesn't prevent leaking in foreign code, that falls under the responsibility of the user, as it would be almost impossible for the compiler to check.

Pointers allocated via `newPtr` are allocated on top of a stack, as the use of higher-order functions forces all pointers to be allocated and freed in a LIFO ordering.
The initial data `a` is then stored in the stack allocation, the given function is called, and then the pointer is freed from the stack.

`newPtr` requires an address for the pointer stack.
In freestanding environments, that address must be defined externally; this can be done using a linker script.
It would be useful if the compiler can be queried for the maximum stack depth so that the user can allocate a sufficiently large section of memory.

## Callbacks

Some foreign functions may accept a pointer to a function to call when a particular event happens.
Callbacks can be supported by wiring additional functionality into pointers.

Function pointers are a nightmare to support, as it involves writing machine code into the pointer stack and making sure that the pointer stack is executable memory, so we choose to only support read-only function pointers.
The problem is that functions written to a pointer might contain free variables, which need to be stored independantly from the stack.
While we can copy those free variables onto the pointer stack, how can the function know where to find them?
Hence, we generate the necessary code on the stack.

In LLVM, this could be achieved using trampoline intrinsics to excise a parameter to a special `eval` function.
The parameter would describe which function needs to be called and what pre-defined parameters need to be added to the call.
Or the LLVM for the function would be generated to use a single `nest` struct containing all the free variables for the function.
These two methods are both to get around LLVM's limitation of one excisable `nest` parameter per function.
Unfortunately, using trampolines requires the compiler to have platform-specific information about the size of the trampoline.

Alternatively, the free variables could be passed through some form of userdata which the foreign function passes to the callback.
Since detecting userdata would be difficult and require special rules in the compiler to handle function pointers, a more reasonable approach would be to add syntax for a function pointer which would require the user to specify a top-level function name with an appropriate type.
Then, the user is responsible for managing the userdata pointer that may or may not be present.

For simplicity, support for callbacks could be omitted entirely.
The user would then be required to write code in a foreign language to create a callback that then calls an exported function and they are free to add whatever userdata they want when doing so.

# Conclusion

Many ideas were explored, however some of them went against the goal of having a minimal language.

With a minimal design, users would use Elemental by declaring foreign functions they wish to use, declaring bindings that compose those foreign functions to manipulate the data that they produce, and then declaring which functions they wish to export to foreign code.
Variables and type variables can be kept as simple as possible by using de Bruijn indices instead of names.

Since the main intention for Elemental is to be generated by a frontend compiler, user convenience features like type synonyms can be omitted for simplicity.
The type system only allows for special constructors: the function arrow and those introduced by the FFI.
The leaf of every Elemental type is a type variable introduced by either a type lambda or a universal type quantification.

Elemental is just System F extended with multiple named bindings and an FFI.

# References

[1] [Functional Programming with Bananas, Lenses, Envelopes, and Barbed Wire](https://maartenfokkinga.github.io/utwente/mmf91m.pdf)  
[2] [Recursive Types for Free (manuscript)](http://homepages.inf.ed.ac.uk/wadler/papers/free-rectypes/free-rectypes.txt)  
[3] [Higher-Order Recursion Abstraction: How to Make Ackermann, Knuth and Conway Look Like a Bunch of Primitives, Figuratively Speaking](https://arxiv.org/pdf/1602.05010.pdf)  
[4] [The Girard-Reynolds Isomorphism](http://homepages.inf.ed.ac.uk/wadler/papers/gr2/gr2.pdf)  
// [5] [Recursive predicates and quantifiers (pages 52 to 53)](https://www.ams.org/journals/tran/1943-053-01/S0002-9947-1943-0007371-8/S0002-9947-1943-0007371-8.pdf)  
[6] [Lambda Calculus Notation with Nameless Dummies, a Tool for Automatic Formula Manipulation, with Application to the Church-Rosser Theorem](http://alexandria.tue.nl/repository/freearticles/597619.pdf)  
[7] [Theorems for free!](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.38.9875&rep=rep1&type=pdf)  
[8] [Is it possible to decide β-equivalence within System F (or another normalizing typed λ-calculus)?](https://cstheory.stackexchange.com/a/14168)
[9] [Imperative functional programming](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.475.2059&rep=rep1&type=pdf)
[10] [Lazy Functional State Threads](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.45.3718&rep=rep1&type=pdf)
