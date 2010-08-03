skim-scheme
===========
skim-scheme is a "simple" dialect of scheme, implementing a subset of the R5RS standard, including:

- TBD

Credits
-------

The skim interpreter is written in Haskell and based on the code from this tutorial: <http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours>

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

 GHC - http://www.haskell.org/ghc/

The 'scm-unit-tests' directory contains unit tests for much of the scheme code. Tests may be executed via 'make test'

The examples directory contains example scheme programs.
