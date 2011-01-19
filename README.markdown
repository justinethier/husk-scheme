![husk Scheme](https://github.com/justinethier/husk-scheme/raw/master/docs/husk-scheme.png)

husk is a dialect of Scheme written in Haskell that implements a subset of the [R5RS standard](http://www.schemers.org/Documents/Standards/R5RS/HTML/). Advanced R5RS features are provided including continuations, hygenic macros, and the full numeric tower.

Husk is not intended to be a highly optimized version of Scheme. Rather, the goal of the project is to provide a tight integration between Haskell and Scheme while at the same time providing a great opportunity for deeper understanding of both languages. In addition, by closely following the R5RS standard the intent is to develop a Scheme that is as compatible as possible with other R5RS Schemes.

Scheme is one of two main dialects of Lisp. Scheme follows a minimalist design philosophy: the core language consists of a small number of fundamental forms, which may be used to implement the other built-in forms. Scheme is an excellent language for writing small, elegant programs, and may also be used to write scripts or embed scripting functionality within a larger application.

Feature List
------------
husk includes the following features:

- Primitive data types and their standard forms, including string, char, numbers (integer, rational, floating point, and complex), list, pair, vector, and symbols
- Proper tail recursion
- Proper lexical scoping
- Conditionals: if, case, cond
- Assignment operations
- Sequencing: begin
- Iteration: do
- Quasi-quotation
- Delayed Execution: delay, force
- Binding constructs: let, named let, let*, letrec
- Basic IO functions
- Standard library of Scheme functions
- Read-Eval-Print-Loop (REPL) interpreter, with input driven by Haskeline
- Full numeric tower - includes support for parsing/storing types (exact, inexact, etc), support for operations on these types as well as mixing types, other constraints from spec.
- Hash tables, as specified by [SRFI 69](http://srfi.schemers.org/srfi-69/srfi-69.html)
- Hygenic Macros: High-level macros via define-syntax - *Note this is still a heavy work in progress* and while it works well enough that many derived forms are implemented in our standard library, you may still run into problems when defining your own macros.
- Continuations - call/cc and first-class continuations. See the change log (release notes) for more information.

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

A Haskell API is also provided to allow you to embed a Scheme interpreter within a Haskell program. The key API modules are:

- Scheme.Core - Contains functions to evaluate (execute) Scheme code.
- Scheme.Types - Contains Haskell data types used to represent Scheme primitives.

For more information, run `make doc` to generate API documentation from the source code. Also, see `shell.hs` for a quick example of how you might get started.

Development
-----------

The following packages are required to build husk scheme:

- [GHC](http://www.haskell.org/ghc/) - Or at the very least, no other compiler has been tested.
- [cabal-install](http://hackage.haskell.org/trac/hackage/wiki/CabalInstall) may be used to build, deploy, and generate packages for husk.
- Haskeline - which may be installed using cabal:

    cabal install haskeline

The 'scm-unit-tests' directory contains unit tests for much of the scheme code. Tests may be executed via 'make test'

The examples directory contains example scheme programs.

Patches are welcome; please send via pull request on github.

Credits
-------

husk scheme is developed by [Justin Ethier](http://github.com/justinethier).

The interpreter is based on the code from the book [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) written by Jonathan Tang and hosted / maintained by Wikibooks.

If you would like to request changes, report bug fixes, or contact me, visit the project web site at [GitHub](http://github.com/justinethier/husk-scheme).

