skim-scheme is a "simple" dialect of scheme, implementing a subset of the [R5RS standard](http://www.schemers.org/Documents/Standards/R5RS/HTML/).  
skim includes the following features:

- Most primitive data types and their standard forms
- Conditionals: if, case, cond
- Assignment operations
- Sequencing: begin
- Quasi-quotation
- Delayed Execution: delay, force
- Binding constructs: let, named let, let*, letrec
- Basic IO functions
- Standard library of Scheme functions
- Hygenic Macros: High-level macros via define-syntax - work in progress
- Hash tables, as specified by [SRFI 69](http://srfi.schemers.org/srfi-69/srfi-69.html) - work in progress 
- Read-Eval-Print-Loop (REPL) interpreter

Features planned for development:

- Iteration: do
- Test cases, including those for: backtick, primitives (see spec section 4.1), others
- Full numeric tower, including support for parsing/storing types (exact, inexact, etc), support for operations on these types as well as mixing types, other constraints from spec.
- Continuations
- Improved REPL features (EG: proper keyboard arrow support, etc)
- More example programs
- General refactoring of haskell code including into multiple modules/files, addressing TODO items, etc...
- Compare skim features to R5RS spec: <http://practical-scheme.net/wiliki/schemexref.cgi?R5RS> and <http://en.wikipedia.org/wiki/Scheme_(programming_language)>

skim-scheme is available under the MIT license.

Usage
-----

The interpreter may be invoked by running it directly from the command line:

    ./skim

Alternatively, you may run an individual scheme program:

    ./skim my-scheme-file.scm


Development
-----------

[GHC](http://www.haskell.org/ghc/) is required to build skim-scheme.

The 'scm-unit-tests' directory contains unit tests for much of the scheme code. Tests may be executed via 'make test'

The examples directory contains example scheme programs.


Credits
-------

skim-scheme is developed by [Justin Ethier](http://github.com/justinethier).

The interpreter is based on the code from the book [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) written by Jonathan Tang and hosted / maintained by Wikibooks.

If you would like to request changes, report bug fixes, or contact the developer of skim-scheme, visit the project web site at [GitHub](http://github.com/justinethier/skim-scheme).

