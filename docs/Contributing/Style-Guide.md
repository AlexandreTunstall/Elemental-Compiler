# Haskell

This repository follows the rules described in [this Haskell style guide](https://kowainik.github.io/posts/2019-02-06-style-guide), except to minimise diffs and enhance readability, there a few differences:

* > ~~The maximum allowed line length is *90 characters*.~~

  The maximum allowed line length is *80 characters*.
  It's already a widely-accepted standard in computer science and many developers have their system configured in a way that 80 is optimal.

* > Do not use ultra-short or indescriptive names like `a`, `par`, `g` unless the types of these variables are general enough.

  An exception is made when the meaning of the variable is clear from the context, since clear short names are better than clear long names. For example, using `t` as a variable for a type is usually fine.

* > Separate end-of-line comments from the code with 2 spaces.

  Avoid writing end-of-line comments whenever doing so does not harm readability.

* > Use block comment style (`{- |` and `-}`) for Haddock in multiple line comments.
  
  Throughout the project, block style comments are written as follows.

  ```haskell
  {-|
      Example of multi-line block comment which is very long and doesn't fit
      single line.
  -}
  ```

* > ~~For commenting function arguments, data type constructors and their fields, you are allowed to use end-of-line Haddock comments if they fit line length limit.~~

  Do not align end-of-line Haddock comments, as this can cause noisy diffs if the type of the function is modified.
  Instead, favour putting the Haddock comment on the previous or next line.

  ```haskell
  replicate
      :: Int
      -- ^ Length of returned list
      -> a
      -- ^ Element to populate list
      -> [a]
  ```

* > If possible, include typeclass laws and function usage examples into the documentation.

  When documenting laws, use [the `prop>` markup](https://www.haskell.org/haddock/doc/html/ch03s08.html#idm140354810771856), as it allows tools like [doctest](https://hackage.haskell.org/package/doctest) to extract and verify them.

* > ~~Use \[stylish-haskell] for automatic module formatting.~~

  You can use any automatic formatter you like as long as you have configured it to follow this style guide.
  Unfortunately, none of the popular automatic formatters seem to support minimising VCS diffs.

* > Write each `LANGUAGE` pragma on its own line, sort them alphabetically ~~and align by max width among them~~.

  Do not align the `#-}` of the `LANGUAGE` pragmas, as this can cause noisy diffs if the used language extension list is modified.

* > ~~You can put commonly-used language extensions into `default-extensions` in the `.cabal` file.~~

  Listing all used language extensions in each file is clearer if the reader has not memorised the list in the .cabal file.

* > **Always write** an explicit export list.

  Having an implicit export list is acceptable in an executable or a test suite.

* > Always use **explicit import lists** or **qualified imports**.

  Implicitly importing a module from the current project is acceptable as it will not cause version-dependant name conflicts.
  Nowadays, a decent code editor using [the Haskell language server](https://github.com/haskell/haskell-language-server) can identify where an imported symbol is defined, so documenting it in the import list is no longer necessary.
  In fact, using explicit import lists can slow down development and increase diff noise, so avoid using them when importing modules from the current project.

* > Imports should be grouped in the following order:
  > 1. ~~Non-qualified~~ imports from Hackage packages.
  > 2. ~~Non-qualified~~ imports from the current project.
  > 3. ~~Qualified imports from Hackage packages.~~
  > 4. ~~Qualified imports from the current project.~~

  With the `ImportQualifiedPost` extension, there is no longer a strong reason to separate non-qualified imports from qualified imports.
  All qualified imports must have `qualified` after the module name in the import.

  Imports from different modules must not be qualified with the same name, as this can also cause name conflicts.

* > ~~Fields of data type constructors should be strict.~~

  Lazy fields are useful in many cases and are thus allowed.
  Profile the application if you think that laziness is affecting performance.

* > Align ~~if,~~ then and else lines with the same level of indentation.

  Often, aligning `if` too results in an awkward-looking early line break.

* > Use the `-XLambdaCase` extension when you perform pattern matching over the last argument of the function.

  Not doing so is acceptable when you need to access the full value of the argument in a non-wildcard alternative because `mx@(Just x)` can sometimes be awkward.

* > Prefer `pure` over `return`.

  In general, prefer the applicative version of a function over the monadic version unless if the two versions behave differently.
  This means:

  * `sequenceA` over `sequence`
  * `traverse` over `mapM`
  * `for` over `forM`
  * `(*>)` over `(>>)`
  * and so on

  In the case of operators, sometimes using the monadic version requires fewer parenthesis due to differing fixities.
  Try to minimise the number of parenthesis used to keep things light and readable, even if it means using the monadic version of a function.

* > Enable `-fhide-source-paths` and `-freverse-errors` for cleaner compiler output.
  > Enable `.hie` files creation for your projects in the `.hie/` directory.

  These options are left up to contributors' preference, as they can be enabled in the `cabal.project.local` file.

# Git

Use descriptive commit messages.
Avoid "Update elemental.cabal"; we can see what files you've changed.
Summarise what changes you've made (e.g. "Improve rewrite system performance") and not how the changes you've made work; that kind of information belongs best in the code, since commit messages are hard to track down, especially when someone else makes small tweaks later and overwrites the Git blame.
In general, [this guide](https://chris.beams.io/posts/git-commit/) explains how to write excellent commit messages.
