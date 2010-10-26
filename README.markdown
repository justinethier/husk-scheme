husk scheme is a "simple" dialect of scheme, implementing a subset of the [R5RS standard](http://www.schemers.org/Documents/Standards/R5RS/HTML/).  
husk includes the following features:

- Most primitive data types and their standard forms
- Conditionals: if, case, cond
- Assignment operations
- Sequencing: begin
- Quasi-quotation
- Delayed Execution: delay, force
- Binding constructs: let, named let, let*, letrec
- Basic IO functions
- Standard library of Scheme functions
- Read-Eval-Print-Loop (REPL) interpreter, with input driven by Haskeline
- Hygenic Macros: High-level macros via define-syntax - work in progress
- Proper tail recursion - work in progress
- Full numeric tower - work in progress, targeted to include support for parsing/storing types (exact, inexact, etc), support for operations on these types as well as mixing types, other constraints from spec.
- Hash tables, as specified by [SRFI 69](http://srfi.schemers.org/srfi-69/srfi-69.html) - work in progress 

Features planned for development:

- Iteration: do
- Continuations
- A scheme "library" that may be used to embed scheme scripting within a Haskell program. This will necessitate extracting the repl code from Core.hs and relocating it to its own file.
- Implementation of approved SRFI's
- Documentation, including a description of Scheme / Haskell, API docs (?), etc

TODO Items:

- Correct parsing of comments, including allowing them in the middle of a form (eg: (cond))
- Test cases, including those for: backtick, primitives (see spec section 4.1), others
- More example programs, perhaps derive some from SICP and http://www.scheme.dk/planet/
- General refactoring of haskell code including into multiple modules/files, addressing of TODO items, etc...
- Compare our features to R5RS spec: <http://practical-scheme.net/wiliki/schemexref.cgi?R5RS> and <http://en.wikipedia.org/wiki/Scheme_(programming_language)>

husk scheme is available under the [MIT license](http://www.opensource.org/licenses/mit-license.php).

Usage
-----

The interpreter may be invoked by running it directly from the command line:

    ./huski

Alternatively, you may run an individual scheme program:

    ./huski my-scheme-file.scm


Development
-----------

The following packages are required to build husk scheme:

- [GHC](http://www.haskell.org/ghc/)
- Haskeline - which may be installed using cabal:

    cabal install haskeline

The 'scm-unit-tests' directory contains unit tests for much of the scheme code. Tests may be executed via 'make test'

The examples directory contains example scheme programs.


Credits
-------

husk scheme is developed by [Justin Ethier](http://github.com/justinethier).

The interpreter is based on the code from the book [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) written by Jonathan Tang and hosted / maintained by Wikibooks.

If you would like to request changes, report bug fixes, or contact the developer of husk scheme, visit the project web site at [GitHub](http://github.com/justinethier/husk-scheme).

