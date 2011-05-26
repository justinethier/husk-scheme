![husk Scheme](https://github.com/justinethier/husk-scheme/raw/master/docs/husk-scheme.png)

husk is a dialect of Scheme written in Haskell that implements a subset of the [R<sup>5</sup>RS standard](http://www.schemers.org/Documents/Standards/R5RS/HTML/). Advanced R<sup>5</sup>RS features are provided including continuations, hygienic  macros, and a full numeric tower.

husk provides many features and is intended as a good choice for non-performance critical applications, as it is not a highly optimized Scheme. Rather, the goal of the project is to provide a tight integration between Haskell and Scheme while at the same time providing an opportunity for deeper understanding of both languages. In addition, by closely following the R<sup>5</sup>RS standard, the intent is to develop a Scheme that is as compatible as possible with other R<sup>5</sup>RS Schemes.

Scheme is one of two main dialects of Lisp. Scheme follows a minimalist design philosophy: the core language consists of a small number of fundamental forms which may be used to implement the other built-in forms. Scheme is an excellent language for writing small, elegant programs, and may also be used to write scripts or embed scripting functionality within a larger application.

Feature List
------------
husk includes the following features from R<sup>5</sup>RS:

- Primitive data types and their standard forms, including string, char, numbers (integer, rational, floating point, and complex), list, pair, vector, and symbols
- Proper tail recursion
- Proper lexical scoping
- Conditionals: if, case, cond
- Sequencing: begin
- Iteration: do
- Quasi-quotation
- Delayed Execution: delay, force
- Binding constructs: let, named let, let*, letrec
- Assignment operations
- Basic IO functions
- Standard library of Scheme functions
- Read-Eval-Print-Loop (REPL) interpreter, with input driven by Haskeline to provide a rich user experience
- Full numeric tower: includes support for parsing/storing types (exact, inexact, etc), support for operations on these types as well as mixing types and other constraints from the R<sup>5</sup>RS specification.
- Continuations: First-class continuations, call/cc, and call-with-values.
- Hygienic Macros: High-level macros via define-syntax - *Note this is still somewhat of a work in progress* and while it works well enough that many derived forms are implemented in our standard library, you may still run into problems when defining your own macros.

As well as the following approved extensions:

- Hash tables, as specified by [SRFI 69](http://srfi.schemers.org/srfi-69/srfi-69.html)

husk scheme is available under the [MIT license](http://www.opensource.org/licenses/mit-license.php).

Installation
------------
husk may be easily installed using [cabal](http://www.haskell.org/cabal/) - just run the following command:

    cabal install husk-scheme

Usage
-----

The interpreter may be invoked by running it directly from the command line:

    ./huski

Alternatively, you may run an individual scheme program:

    ./huski my-scheme-file.scm

API
---

A Haskell API is also provided to allow you to embed a Scheme interpreter within a Haskell program. The key API modules are:

- `Language.Scheme.Core` - Contains functions to evaluate (execute) Scheme code.
- `Language.Scheme.Types` - Contains Haskell data types used to represent Scheme primitives.

For more information, run `make doc` to generate API documentation from the source code. Also, see `shell.hs` for a quick example of how you might get started.

Foreign Function Interface
--------------------------

A foreign function interface (FFI) is provided to allow husk to call into arbitrary Haskell code. This allows us to call into new Haskell code without having to modify husk. The interface is currently available via the `load-ffi` function:

    (load-ffi "Language.Scheme.Plugins.CPUTime" "precision" "cpu-time:precision")

This function takes the following string arguments:

- Haskell module to dynamically load
- Function to load from that module
- Name to use for the function after it is loaded into husk

See husk's CPUTime module for an example of how to use the husk FFI.

Development
-----------

The following packages are required to build husk scheme:

- [GHC](http://www.haskell.org/ghc/) - Or at the very least, no other compiler has been tested.
- [cabal-install](http://hackage.haskell.org/trac/hackage/wiki/CabalInstall) may be used to build, deploy, and generate packages for husk.
- Haskeline - which may be installed using cabal: `cabal install haskeline`

The `scm-unit-tests` directory contains unit tests for much of the scheme code. All tests may be executed via `make test` command.

The examples directory contains example scheme programs.

Patches are welcome; please send via pull request on github.

Credits
-------

husk scheme is developed by [Justin Ethier](http://github.com/justinethier).

The interpreter is based on code from the book [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) written by Jonathan Tang and hosted / maintained by Wikibooks.

If you would like to request changes, report bug fixes, or contact me, visit the project web site at [GitHub](http://github.com/justinethier/husk-scheme).

