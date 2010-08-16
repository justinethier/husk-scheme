skim-scheme
===========
skim-scheme is a "simple" dialect of scheme, implementing a subset of the R5RS standard. skim includes the following features:

- Most primitive data types and their standard forms
- Conditionals if, case, cond
- Assignment operations
- Sequencing
- Quasi-quotation
- Basic IO functions
- Read-Eval-Print-Loop (REPL) interpreter
- Standard library of Scheme functions, including many common functions
- Hygenic Macros (work in progress)
- Delayed Execution (work in progress)

Features under development:

- do, let*, letrec, others?
- Full numeric tower
- Other missing sections from 4.2.*
- Continuations
- Improved REPL features
- More example programs
- Refactoring of haskell code into multiple modules/files
- Compare skim features to <http://practical-scheme.net/wiliki/schemexref.cgi?R5RS> and <http://en.wikipedia.org/wiki/Scheme_(programming_language)>

License
-------
Copyright (c) 2010 Justin Ethier

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

Credits
-------

The skim interpreter is based on the code from the book "Write Yourself a Scheme in 48 Hours" written by Jonathan Tang and hosted / maintained by Wikibooks: <http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours>

If you would like to request changes, report bug fixes, or contact the developer of skim-scheme, visit the project web site at <http://github.com/justinethier/skim-scheme>

Usage
-----

The interpreter may be invoked by running it directly from the command line:

    ./skim

Alternatively, you may run an individual scheme program:

    ./skim my-scheme-file.scm


Information
------------

The following packages are required to build skim-scheme:

- GHC - http://www.haskell.org/ghc/

The 'scm-unit-tests' directory contains unit tests for much of the scheme code. Tests may be executed via 'make test'

The examples directory contains example scheme programs.
