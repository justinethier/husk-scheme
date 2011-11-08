v3.4
----
This release adds the first hygienic macro support to husk. There are two
"sides" to hygiene:

- names that are introduced by the macro do not clash with names in sub-expressions that are passed from user code to it
- names that the macro references are coming from the lexical context of its definition, rather than the lexical context of its use

Support has been added for both sides, although there are some issues as noted
in the [Version 3.4.x Milestone](https://github.com/justinethier/husk-scheme/issues?milestone=9&state=open). Macro support will continue to improve in future releases.

In addition, this release contains the following bug fixes:

- When searching for a redefinition of a special form, the code now
recursively examines parent environments instead of only inspecting the
current environment.
- (let*) is now defined in terms of the definition from R<sup>5</sup>RS, to
fix issues with valid inputs that were not matched by (let*).

v3.3
----
This release includes major improvements to the macro module.
In particular:

- husk now supports arbitrary nesting levels of macro 0-or-many matches (IE: `...`). Previous versions would not handle macros that had more than 2 nesting levels of 0-or-more matches.
- Macros now correctly handle improper lists. Previous versions of husk did not respect the fact that the trailing item in an improper list may be matched zero or more times. Also, husk macros now properly convert the transformed code into improper or proper lists depending upon the pattern, input, and transform.
- (do) is now defined in terms of the macro from R<sup>5</sup>RS.

Other changes:

- Updated the parser to simplify improper lists; for example the following input: `'(1 2 . (3 . ()))` will now be converted to `(1 2 3)`
- Corrected the order of arguments to (fold). For example, the expression (fold (lambda (x y) x) 8 (list 1 2 3)) should yield 3, however in previous releases husk would incorrectly yield 8 because of the order of arguments.

v3.2.1
------
- Added a conditional compilation check to allow husk to build on GHC 6 and GHC 7.

v3.2
----
- Significant improvements to the parser, including proper handling of whitespace and comments. This represents a critical upgrade from the previous releases.
- Added support for nested block comments using `#|` and `|#`, per the R<sup>7</sup>RS draft.
- Added `hash-table-fold`

v3.1
----
- Fixed issues with lexical scope, where proper scope was not observed for
special forms, including: if, cond, set!, begin, define, lambda, set-car!,
set-cdr!, string-set!, vector-set!, and hash-table-set! - For example: 

`(let ((if +)) (if 1 2 3))` was 2 instead of 6. Now it eval's to 6

- `cond`, `and`, and `or` are now defined using the corresponding macros from R<sup>5</sup>RS,
  to give them the expected semantics.
- Added a `gensym` primitive.
- Internal change - relocated primitive functions to a new Language.Scheme.Primitives module.
- Marked macro support as non-hygienic since hygiene is not fully supported at present.

v3.0
----
- Added a foreign function interface (FFI) to allow husk to call directly into Haskell code.

v2.4
----
- Simplified the core evaluator by moving several functions out of eval.
Practically, this means that the following functions are now first-class objects:
apply, call-with-current-continuation, call-with-values, eval, and load. 
They can be called directly, assigned to variables, etc just like any
other first-class value. 
- (call-with-values) now accepts multiple return values from the producer function.
- If (values) is called with multiple arguments and is not part of an
enclosing (call-with-values), the first value is returned to the interpreter
instead of generating an error.
- Added additional error handling for set! - and related functions - when these
functions are called with the wrong number or type of arguments.
- Added limited support for (dynamic-wind).
- The parser now recognizes rational number components of a complex number.
For example: 1/2+3/4i

v2.3
----
- Automatically load the scheme standard library when running .scm files.
- Suppress excess output when running a program by piping it to /dev/null.
- Added missing I/O functions, including display, input-port?, output-port?,
newline, write-char, read-char, peek-char, current-input-port,
current-output-port, call-with-input-file, and call-with-output-port.
- Added eval.
- Added a limited version of (call-with-values) that currently only accepts
one argument.

v2.2
----
- Added vector support to macros.
- Added an example program to demonstrate file I/O: examples/simple-file-io.scm.

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
