# The Elemental Compiler

The Elemental programming language is a total programming language that I've designed to be as simple as possible while still being surprisingly powerful.
As a superset of System F, it can be used to compute any primitive recursive function and even a number of general recursive functions, such as Ackermann's function [1].

What can't it compute? I don't know yet.
If you have a useful function that can't be computed in System F (or you're unsure about), I'd love to hear what it is and why it's useful, so please create an issue to open a discussion around it.

This project is a compiler for the Elemental language.
In contrast with most existing functional languages, Elemental programs built with this compiler require no runtime system, no garbage collector, and no heap.
Although there is manual memory management, it is only required for FFI, and advanced data structures which in typical systems languages rely on the heap here are automatically handled by the compiler.
The compiler also performs whole program optimisation.

In summary, the language is:

* General-purpose - not designed for use in a specific domain.
* Total - every program is guaranteed to terminate (if we ignore abusing the FFI with non-total foreign code).
* Purely functional - functions are first-class citizens and are used in a declarative compositional style.
* Referentially transparent - references to variables can be replaced with the value of the variable without altering behaviour.

The language's type system is:

* Static - all terms have a type determined during compilation.
* Strong - all types are checked during compilation and there is no coercion or casting.
* Sound - there are no type errors at runtime.
* Structural - types are deemed equivalent based on their structure.
* Parametrically polymorphic - polymorphic functions must handle all polymorphic data as opaque and cannot have different implementations based on what type parameter they're given.
* Impredicatively polymorphic - polymorphic functions are first class citizens and polymorphic types can be used as type parameters to other polymorphic types.

Programs compiled with this implementation are:

* Freestanding - programs can run in a bare metal environment without a host operating system.
* Runtime-free - programs don't need to include a runtime system or a garbage collector.
* Stack-based - programs need a stack to run (mostly to spill registers).

## Why?

Most existing total programming languages are built to use a non-total programming language compiler like C or Haskell as a backend.
This means that they can't take full advantage of their lack of Turing completeness, which is the main reason this compiler does not require a heap.

By making Elemental as simple as possible, it becomes relatively easy to compile and can be used as a backend for more complex total programming languages.

## Syntax & Semantics

A word of warning: if the language seems verbose, that's because it is!
The majority of the code in a program is there to tell the compiler about the types of expressions and defer type inference and other human-friendly features to whatever's generating the code.
Since a frontend compiler already knows all the types, passing these types to the Elemental compiler is a simple task.
Sacrificing human convenience in this way massively simplifies implementing the type system.

Every Elemental program is a UTF-8 encoded text file containing a list of declarations separated by semicolons or newlines.
The language also follows an indentation layout rule similar to Haskell's, where everything on a newline is part of the current declaration if there's whitespace at the start of the line.

For example programs, see the files in `test/Golden`.
For a more complete explanation of the syntax, see the AST in `src/Language/Elemental/AST/*.hs` and the parser in `src/Language/Elemental/Parser.hs`.

### Declarations

There are multiple different types of declarations, most of which are related to the foreign function interface (FFI).

* Bindings, to assign expressions to variables.

  ```
  [name] = [expr]
  ```

  For example:

  ```
  not = λ(∀ 0 → 0 → 0) Λ λ0 λ0 2 @0 0 1
  ```

* Foreign imports, to call foreign functions (e.g. C functions).

  ```
  foreign import [name] "[foreign_name]" : [type]
  ```

  Here, `[type]` must be a foreign type.
  For example:

  ```
  -- In C, this would be declared: void putchar(char);
  -- (Note that the type is simpler than libc putchar.)
  foreign import c_putchar "putchar"
    : (∀ ((∀ 0 → 0 → 0) → (∀ 0 → 0 → 0) → (∀ 0 → 0 → 0) → (∀ 0 → 0 → 0)
        → (∀ 0 → 0 → 0) → (∀ 0 → 0 → 0) → (∀ 0 → 0 → 0) → (∀ 0 → 0 → 0)
        → 0) → 0)
    → IO (∀ 0 → 0)
  ```

* Foreign exports, to expose Elemental definitions to foreign code.

  ```
  foreign export "[foreign_name]" [expr] : [type]
  ```

  Here, `[type]` must be a foreign type.
  For example:

  ```
  -- Let's pretend that mkChar, f, and t are defined.
  -- In C, this can be declared and used as: void main(void);
  foreign export "main" (c_putchar (mkChar f t f f t f f f)) : IO (∀ 0 → 0)
  ```

* Foreign primitives, to access FFI functionality wired into the compiler itself.

  ```
  foreign primitive [name] : [type]
  ```

  Here, `[name]` and `[type]` must be the name and type of the primitive.

  ```
  foreign primitive pureIO : ∀ 0 → IO 0
  ```

* Foreign addresses, to read and write static addresses at runtime (note that this may require careful linking).

  ```
  foreign address [name] [addr] : [type]
  ```

  Here, `[type]` must be a `ReadPointer` or a `WritePointer` of a marshallable type.
  For example:

  ```
  -- The physical memory address of the VGA text buffer in x86.
  foreign address vga_buf_start 0xb8000 : ReadPointer
    (∀ ((∀ 0 → 0 → 0) → (∀ 0 → 0 → 0) → (∀ 0 → 0 → 0) → (∀ 0 → 0 → 0)
    → (∀ 0 → 0 → 0) → (∀ 0 → 0 → 0) → (∀ 0 → 0 → 0) → (∀ 0 → 0 → 0)
    → 0) → 0)
  ```

The FFI declarations have constraints on what their type can be.
See the types section for information about foreign types and marshallable types.

### Expressions

Expressions are composed of:

* References, which reference a declaration by its name.

* Term variables, which reference a variable in scope by its de Bruijn index or, in other words, how many variables are introduced between the variable's introduction and its reference.

  For example:

  ```
  -- The third 0 references the variable introduced by the innermost lambda.
  f = Λ λ0 λ0 0

  -- The 1 references the variable introduced by the second innermost lambda.
  t = Λ λ0 λ0 1
  ```

* Term applications, which apply a function term (left) to a variable term (right).

  ```
  [func] [arg1] [arg2]
  ```

  For example:

  ```
  -- This should be true, because ¬F = T.
  bool_property = bool_eq (not f) t
  ```

* Term abstractions, which define a function and introduce a variable for the argument of the function.

  ```
  λ[type] [expr]
  ```

  For example:

  ```
  -- The first two lambdas have booleans as arguments.
  -- Their arguments can then be referenced as "2" and "3".
  bool_eq = λ(∀ 0 → 0 → 0) λ(∀ 0 → 0 → 0) Λ λ0 λ0 2 @0 (3 @0 1 0) (3 @0 0 1)
  ```

* Type applications, which apply a type function term (left) to a type variable term (right).

  ```
  [type_func] @[type]
  ```

  For example:

  ```
  -- Do something different based on whether cond is true or false.
  -- Here, from (cond : ∀ 0 → 0 → 0), we have:
  -- cond @(IO (∀ 0 → 0)) : IO (∀ 0 → 0) → IO (∀ 0 → 0) → IO (∀ 0 → 0)
  main = cond @(IO (∀ 0 → 0)) when_true when_false
  ```

* Type abstractions, which define a type-polymorphic function and introduce a type variable for the argument of the function.

  ```
  Λ [expr]
  ```

  For example:

  ```
  -- Polymorphic identity function. Here, we have (id : ∀ 0 → 0)
  -- The type abstraction introduces a type variable which is then used as the
  -- type of the term abstraction.
  id = Λ λ0 0
  ```


Expressions are reduced to equivalent expressions following the same β-reduction rules as System F:

* `(λ[type] x) y` is equivalent to `z` where `z` is `x` with variable `0` substituted with `y`.
* `(Λ x) @y` is equivalent to `z` where `z` is `x` with type variable `0` substituted with `y`.

### Types

Types are composed of:

* Type variables, which reference a type variable in scope by its de Bruijn index or, in other words, how many type variables are introduced between the type variable's introduction and its reference. They use the same syntax as term variables.

* Function arrows, which correspond to the type of term abstractions, where the LHS is the argument type and the RHS is the return type.
  
  ```
  [arg1] → [arg2] → [ret]
  ```

* Universally quantified types, which correspond to the type of type abstractions and introduce a type variable to the scope.

  ```
  ∀ [type]
  ```

There are also types used solely for FFI:

* `IO` types, which correspond to the return type of all imported and exported foreign functions.

  ```
  IO [type]
  ```

  A term of type `IO x` does not actually "hide" a term of type `x` but rather describes a way of getting a value of type `x` from the surrounding environment.
  As such, there is no `IO x → x` function.
  Instead, `IO` terms are composed via the `bindIO` primitive.

* `ReadPointer` and `WritePointer` types, which respectively correspond to the types of readable and writable pointers.

  ```
  ReadPointer [type]
  WritePointer [type]
  ```

Foreign types are types where all the argument types are marshallable and the return is an `IO` type of a marshallable type.

The marshallable types are the following:

* The unit type `∀ 0 → 0`
* The boolean type `∀ 0 → 0 → 0`
* A tuple of two or more booleans `∀ ([...] → (∀ 0 → 0 → 0) → (∀ 0 → 0 → 0) → 0) → 0`

### Primitives

As part of the FFI, the compiler wires in some primitives to make use of the FFI types.

* `pureIO : ∀ 0 → IO 0` (lift a value into `IO`)
* `bindIO : ∀ IO 0 → ∀ (1 → IO 0) → IO 0` (compose two `IO` values, using the result of the first to get the second)
* `loadPointer : ∀ ReadPointer 0 → IO 0` (load the value at the pointer)
* `storePointer : ∀ WritePointer 0 → 0 → IO (∀ 0 → 0)` (store the value in the pointer)

## Usage

Currently, the compiler is only available as a library.
Documentation for the library can be found in the Haddock-generated documentation.

If you want to try out the compiler without writing the necessary code to use the library, the easiest way of doing so is by abusing the test suite:

1. Clone this repository
2. Add the Elemental program files you want to compile to `test/Golden`. They must have a `.elem` file extension, otherwise the test suite will ignore them.
3. Install Cabal and GHC. The easiest way of getting versions of these tools that can build the project is to [install Nix](https://nixos.org/download.html) (if on Windows, you'll need to use [WSL](https://docs.microsoft.com/en-us/windows/wsl/)) and run `nix-shell` to open a new shell with all the necessary tools.
4. Run the test suite with `cabal v2-test`. If your program is valid, this should generate an unoptimised `.ll` file and an optimised `.opt.ll` file, which can then be used with LLVM tools such as `llc`. Otherwise, you'll get an error describing why your program was rejected.

If you run into any bugs or usability problems, please open an issue in [the issue tracker](https://github.com/AlexandreTunstall/Elemental-Compiler/issues).

## How does it work?

After parsing the program and verifying its correctness, the compiler evaluates the program.
The result of this evaluation is the output of the compiler.

This comes with the benefit that the compiler will always fold constant values together as much as it can, meaning that things that can be computed at compile-time will never be computed at runtime.

However, this also means that the compiler might try to unroll very large loops, which could lead to poor compile-time performance or large code size.
Since they're both compile-time issues, they are not major concerns of this project, however the compiler has nonetheless been carefully written for good compile-time performance and changes that resolve them are still welcome.

## Licensing & Contributing

If you'd like to use this project or contribute to it, please open an issue to ask me to set a license (suggestions on which licenses to use/avoid are welcome).

Suggestions and improvements for the FFI are especially welcome, because it is currently very inflexible.
Unfortunately, finding an elegant way of supporting a wide array of foreign operations without violating safety is no easy task.

## References

[1] [*Higher-Order Recursion Abstraction: How to Make Ackermann, Knuth and Conway Look Like a Bunch of Primitives, Figuratively Speaking*, Baltasar Trancón y Widemann](https://arxiv.org/abs/1602.05010)
