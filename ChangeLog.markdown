v3.5.7
--------

The major change in this release is support for explicit renaming macros. This low-level macro system provides the ability to break macro hygiene, if necessary.

TODO: need the following for this release:
 - TODO items from er-macro.scm
 - letrec-syntax and let-syntax need to recognize the er transformer
 - anything else?

v3.5.6
--------

- Enhanced the compiler to accept `load-ffi` as a special form, so a compiled version of a program does not have to wait for a module to be dynamically loaded. Instead, the module is included at compile time. This offers a nice speed improvement:

  <pre>
  <code>
     $ time huski ffi-cputime.scm 
     Seconds of CPU time spent: 2.756171

     $ time ./ffi-cputime 
     Seconds of CPU time spent: 2.4001e-2
  </code>
  </pre>

- Allow a hash table to be defined directly using `#hash(alist)` - for example, `#hash()` for an empty table or `#hash((a 1) (b . 2))` for a table with two elements. This is not part of R<sup>5</sup>RS but seems less clumsy than the standard way, and a similar language feature is provided by Racket.
- Added support for `hash-table-set!` and `hash-table-delete!` to the compiler.
- Fixed a bug where `integer?` would always return false for negative rational numbers. For example, `(integer? -2/2)` should evaluate to `#t`.

v3.5.5
--------

- Added support for SRFI-1 (List Library), which can be loaded using `(require-extension (srfi 1))`. Note that linear update functions (such as `map!`, `take!`, etc) and circular lists are not supported at this time.
- Added a new LispVal type called `Opaque` for Haskell integration, courtesy of Josh Triplett. The Opaque type allows a native Haskell function to package an arbitrary Haskell type for use by other native code called by husk. See `examples/ffi/Opaque.hs` for an example of how to use this feature.
- Implemented `file-exists?`, `delete-file`, `char-ready?`, `rationalize`, `gcd`, and `lcm`.
- Enhanced the parser to read numbers in scientific notation such as `1e3` and `4.2e1`.
- Modified `numerator` and `denominator` to add support for integers and floating point numbers.
- Fixed a bug in `set-cdr!` where an unsimplified list may be output. For example, `(3 . (2 . (1 . ())))` instead of `(3 2 1)`.
- Allow `apply` to receive multiple arguments, as long as the last one is a list.

v3.5.4
--------

- Enhanced `huski` and `huskc` to print errors to console when a runtime exception occurs,
and to not print any results to console unless the user calls an I/O function
such as `write` or `display`.
- Added a special form `expand` that can be used to see the result of a macro
expansion, for debugging purposes. For example: `(expand (let ((x 1)) x))`.
- Allow a list to be enclosed by matched brackets as well as parentheses. For example: `(+ 1 2 [+ 3 4])`.
- Internal change - cleaned up code by using Language pragmas instead of explicitly using extensions in the cabal and make files.

v3.5.3.2
--------

- Modified huski to escape baskslashes in the path to the standard library, to guarantee stdlib is loaded when husk is installed on a Windows machine.

v3.5.3.1
--------

- Removed an unnecessary dependency on `/dev/null` to allow `huski` to interpret files when run on Windows.
- Prevent divide-by-zero errors when dividing by a single number, or processing a rational number with zero as the denominator.

v3.5.3
------
This release adds full support for GHC 7.2.2 / 7.4.1 as well as a number of small enhancements.

- Fixed the FFI to work in both GHC 7.2.2 and GHC 7.4.1.
- Implemented SRFI 23, which adds error reporting via the `(error)` function.
- Added support for `let-syntax`, `letrec-syntax`, `set-car!`, `set-cdr!`, `vector-set!`, and `lambda` (pair syntax) to the huskc compiler.
- Added the `--dynamic` option to the compiler to use dynamic Haskell libraries.
- Added the `--extra` option to the compiler to allow passing of arbitrary arguments directly to ghc.
- Fixed huski to allow any redefinition of `let-syntax` and `letrec-syntax`.

v3.5.2.x
------
This is a series of quick bug-fix releases that allows husk to build under GHC 7.4.1.

v3.5.2
------
- Added an experimental compiler, huskc. For more information see [Issue #62](https://github.com/justinethier/husk-scheme/issues/62).
- Streamlined the cabal file so that each source file is only compiled a single time.

v3.5.1
------
- Improved support for comparing instances of functions using `eq?`, `eqv?`, etc.
- Reduced variable access time by using a Map to store variables within an environment. 
- Various internal changes such as renaming the tests directory and integrating R<sup>5</sup>RS pitfalls with the unit tests.

v3.4.4
------
This release continues the trend of quick point releases for the 3.4.x series. The key change is support for GHC 7.2: 

- husk now compiles in GHC 7.2. This ended up being a simple change - I had mis-interpreted the value of a pre-processor directive. Sorry about that!
- Replaced the definition of `letrec` with the macro from R<sup>5</sup>RS.
- Allow a continuation to call into another continuation via call/cc - see R<sup>5</sup>RS pitfall 1.2

v3.4.3
------
- Fixed `let-syntax` and `letrec-syntax` to prevent conflicts between identifiers of the same name in the outer scope and within the macro body.
- Per R5RS, `(if <test> <consequent>)` is supposed to evaluate `<consequent>` for any value of `<test>` except `#f`. However, husk was only allowing `<test>` to pass if it was equal to `#t`. This has been fixed.
- Modified `(append)` to accept an arbitrary number of arguments, per R<sup>5</sup>RS.
- Replaced the macro for `case` with the one from R<sup>5</sup>RS.

v3.4.2
------
- Many improvements and bug fixes for let-syntax and letrec-syntax.
- Improved handling of define and set! within a macro definition.

v3.4.1
------
Added experimental support for let-syntax and letrec-syntax.

Bug fixes:

- Fixed a nasty issue where the macro pattern matching code would not properly reset itself when transcribing a macro containing an ellipsis in the middle of a list. For example: `(a ... b c)`. This caused the expanded macro to be incomplete and/or malformed.
- Fixed a bug that would cause the evaluator to terminate if define-syntax was called within a nested scope. 

v3.4
----
This release adds the first hygienic macro support to husk. There are two "sides" to macro hygiene:

- Hygiene: Names that are introduced by the macro do not clash with names in sub-expressions that are passed from user code to it
- Referential transparency: names that the macro references are coming from the lexical context of its definition, rather than the lexical context of its use

Support has been added for both sides, although there are some issues as noted in the [Version 3.4.x Milestones](https://github.com/justinethier/husk-scheme/issues?milestone=9&state=open). Macro support will continue to improve in future releases.

In addition, this release contains the following bug fixes:

- When searching for a redefinition of a special form, the code now
recursively examines parent environments instead of only inspecting the
current environment.
- `(let*)` is now defined in terms of the definition from R<sup>5</sup>RS, to
fix problems with valid inputs not being matched by `(let*)`.

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
