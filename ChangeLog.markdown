v2.3
----
- Automatically load the scheme standard library when running .scm files
- Suppress excess output when running a program by piping it to /dev/null
- Added missing I/O functions, including display, input-port?, output-port?

v2.2
----
- Added vector support to macros
- Added an example program to demonstrate file I/O: examples/simple-file-io.scm

v2.1
----
- Moved all Haskell code from the 'Scheme' namespace to the 'Language/Scheme'
namespace.
- Fixed cases where backquoting of a vector would crash the interpreter.
- Prevent interpreter from crashing when calling (load) if the file does not
exist.
- Improved support of different numeric types across the numerical functions.
- Added support for binary numbers to (number->string)
- Implemented (alist->hash-table)

v2.0
----
- Full implementation of continuations via call/cc. This involved passing a new continuation type through all versions of the eval function.
- Fixed a bug where a macro pattern would be incorrectly matched even though a literal identifier in the pattern was not present in the input.
- Added over 100 new Scheme test cases from the R5RS language specification.
- Added set-cdr!
- Changed the banner text on startup to match the new husk logo.

v1.3
----
- Added limited support for continuations, by adding the call/cc keyword and first-class continuations. This is still a work in progress, and only a subset of continuations are supported - continuations can only be used as escape procedures from a function (along the lines of a 'return' call).

v1.2
----
- Fixed an ugly bug where the underlying functions used to mutate variabiables in (define), (set!), etc implemented dynamic scoping instead of the lexical (static) scoping required by R5RS. [This reference implementation](http://web.mit.edu/kmill/Public/lilscheme.hs) written by Kyle Miller was used as a starting point.

v1.1
----
- Many improvements to quasi-quotation, including support for unquote splicing (,@).

v1.0
----
- Initial release
