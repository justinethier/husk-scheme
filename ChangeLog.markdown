v3.17.1
--------

- Improved passing of extra arguments within the interpreter (TODO: a bit vague? may want to check changes here)
- Reduced size of compiled code by ??? % (TODO: get more information here, also can more improvements be made?)
- Added `call-with-port` from R<sup>7</sup>RS.

Bug Fixes:

- Added error checking to many I/O functions to prevent crashes when using a port that has already been closed.
- Added optional start/end arguments to `string->vector`, `vector->string`, `vector-copy!`, and `string-copy!`.

v3.17
--------

- Added support for [`define-record-type`](http://justinethier.github.io/husk-scheme/manual/node57.html) from R<sup>7</sup>RS and SRFI 9. This syntax allows creation of new disjoint types supporting access to multiple fields.
- Added support for parameter objects from R<sup>7</sup>RS and SRFI 39. See [dynamic bindings](http://justinethier.github.io/husk-scheme/manual/node41.html) in the user manual for more information.
- Added a `(scheme process-context)` library containing the following functions:
     - [`emergency-exit`](http://justinethier.github.io/husk-scheme/manual/node86.html#emergency-exit)
     - [`exit-fail`](http://justinethier.github.io/husk-scheme/manual/node86.html#exit-fail)
     - [`exit-success`](http://justinethier.github.io/husk-scheme/manual/node86.html#exit-success)
     - [`get-environment-variable`](http://justinethier.github.io/husk-scheme/manual/node86.html#get-environment-variable)
     - [`get-environment-variables`](http://justinethier.github.io/husk-scheme/manual/node86.html#get-environment-variables)
     - [`system`](http://justinethier.github.io/husk-scheme/manual/node86.html#system)

Bug Fixes:

- Fixed a macro bug where the last element of a pattern's improper list may not be matched correctly if there is an ellipsis earlier in the list.
- Prevent infinite recursion when evaluating a pointer that contains a pointer to itself.
- Fixed the compiler to add full support for splicing of `begin` definitions.
- Updated `dynamic-wind` to return the value from the `during` thunk instead of the `after` thunk.

v3.16.1
--------

- Allow import of a library in the same directory as a program. For example to import `lib.sld`:

        (import (lib))

Bug Fixes:

- Husk no longer throws an error during expansion of a library macro that references another macro which is not exported from the same library.
- Fixed a bug where a `syntax-rules` macro's literal identifier would not match the input when both identifiers are equal and both have no lexical binding.

v3.16
--------

- Improved import of libraries:

    - Husk now detects cyclic dependencies and throws an error instead of going into an infinite loop.
    - Each library is only evaluated once during the import process.

- `begin` now has the ability to evaluate contained expressions and definitions as if the enclosing `begin` were not present, per R<sup>7</sup>RS. For example:

        huski> x
        Getting an unbound variable: x
        huski> (begin (define x 28) x)
        28
        huski> x
        28

- Added the following R<sup>7</sup>RS I/O functions: 

    - `get-output-bytevector`
    - `get-output-string`
    - `open-input-bytevector`
    - `open-input-string`
    - `open-output-bytevector`
    - `open-output-string`
    - `read-string`
    - `write-string`

- Added an `-i` command line option to `huski`. This option will start the interactive REPL after a file specified on the command line is executed, and has no effect if no file is specified.

Haskell API:

- The `Port` data type has been extended to include an optional in-memory buffer:
 
        Port Handle (Maybe Knob)

  These changes are isolated in husk, but if your code uses any `Port` constructors, you would need to change them, EG: `Port _ Nothing`.


v3.15.2
--------

Bug fixes:

- The `(husk random)` library's `randint` function no longer throws a runtime error when called.
- The `newline` function now accepts a port as an optional second argument, instead of throwing an error.

v3.15.1
--------

This is a small bug fix release:

- Preserve macro hygiene when using code that contains explicit renaming macros contained within syntax-rules macros. Previously, the syntax-rules system would not pass renamed variables across to the ER system. So an identifier could be renamed by syntax-rules but the ER macro would then have no knowledge of the rename and would be unable to use `rename` to make the identifier hygienic. For example, the code:

        (let ((unquote 'foo)) `(,'bar))

 Should evaluate to `((unquote (quote bar)))`.

- Added support for multi-line input to `huski`.
- Fixed GHC compiler warnings when building with `-Wall`.

v3.15
--------

The big change for this release is an online [User Manual](http://justinethier.github.io/husk-scheme/manual/index.html) based on the R<sup>7</sup>RS, that documents Husk's Scheme and Haskell APIs, and explains the Scheme language as implemented by Husk. This is going to be a work-in-progress but is mostly complete, and can be used immediately as a reference.

In addition, many smaller fixes and enhancements are included:

- Improved library support so that examples no longer require running in a special mode.
- Added missing functions to `(scheme base)`.
- Added support for the `(scheme case-lambda)` and `(scheme r5rs)` libraries.
- Added libraries for SRFI 1, 2, and 69 so that these features are available via `import`. 
    - For example: `(import (srfi 1))`.
- Added `exact-integer-sqrt` from R<sup>7</sup>RS, using the Chibi scheme reference implementation.
- Added `let-values` and `let*-values` from R<sup>7</sup>RS.
- Added the following I/O functions:
    - `binary-port?`
    - `close-port`
    - `input-port-open?`
    - `open-binary-input-file`
    - `open-binary-output-file`
    - `output-port-open?`
    - `read-bytevector`
    - `textual-port?`
    - `u8-ready?`
    - `write-bytevector`
- Allow character and string comparison predicates (such as `string=?` and `char=?`) to support more than two arguments.
- Fixed `cond-expand` to support `and` / `or` clauses.
- Renamed `char-upper` and `char-lower` to `char-upcase` and `char-downcase` to match the Scheme specs.

v3.14
--------

Made the following enhancements to improve R<sup>7</sup>RS support:

- Updated the parser for strings, symbols, and character literals. The reader now understands Unicode hex values, strings and symbols now allow mnemonic and numeric escape sequences, and the list of named characters has been extended.
- Added support for the `=>` syntax to the `case` conditional.
- Added `cond-expand` syntax to statically expand different expressions depending upon whether features are present in the Scheme implementation. This could help allow the same code to work in husk as well as other R<sup>7</sup>RS implementations.
- Added an optional second argument to `log` to allow specifying the base.
- Added the following functions: `nan?`, `finite?`, `infinite?`, `exact-integer?`, `exact?`, `inexact?`, `square`, `boolean=?`, `symbol=?`, `read-line`, `flush-output-port`, `eof-object`.
- Added support for `include`, `letrec*`, `syntax-error`, `unless`, and `when` syntax.

Added a library to compute simple random numbers, based on [this Stack Overflow answer](http://stackoverflow.com/a/14675103/101258):

    (import (husk random))
    (random num) ; Seed the RNG
    (random) ; Random number from 0 to 1
    (randint lo hi) ; Generate random integer between lo (optional) and hi

v3.13
--------

- Added the command line flag `--revision 7` (or `-r7` for short) to allow huski and huskc to start in R<sup>7</sup>RS mode.
- Added most of the standard R<sup>7</sup>RS libraries: `(scheme base)`, `(scheme char)`, etc.
- Extended syntax-rules to allow another identifier to be used to specify the ellipsis symbol, per R<sup>7</sup>RS. For example, `:::` could be used instead:

        (define-syntax and
          (syntax-rules ::: ()
            ((and test1 test2 :::)
             (if test1 (and test2 :::) #f))))

- Added the following functions from R<sup>7</sup>RS: `make-list` , `list-copy` , `list-set!` , `vector-copy` , `vector-map` , `vector-for-each` , `vector-append` , `string-map` , `string-for-each` , `string->vector` , `vector->string` , `vector-copy!` , `string-copy!` 

v3.12
--------

Significant enhancements have been made to the huski REPL:

- Allow using huski to run Scheme scripts from the shell, as specified by SRFI 22. The script needs to start with the line `#! /usr/bin/env huski` or equivalent, and a `main` function may be defined to receive command line arguments. The `examples/scripts` directory contains example programs `cat.scm` and `sum.scm` that demonstrate how this works in practice.
- Add tab completion for Scheme variables and special forms. Tab completion will still fill in filenames when tab is pressed within double-quotes. This makes it easy to find a file in certain cases such as for a `load`.
- Accept (and ignore) inputs of just whitespace. Previously this would display a nasty error message.

This release also includes the following features:

- Added the `(scheme time)` library from R<sup>7</sup>RS.
- Added the `system` function to make system calls from a husk program. The syntax is `(system "command")`. An integer status code is returned with the same value that the executing program returned to the OS.

Bug fixes:

- Duplicate or otherwise invalid lambda parameters are not allowed, and will throw an error. For example: `(lambda (a a 1) a)`
- It is now a parse error to have a form that includes an empty car cell, for example: `'( . 1)`
- Ensure all of husk's exports are included in the Haskell API documentation.

v3.11
--------

The major change in this release is support for R<sup>7</sup>RS style library syntax in the compiler. This enables functionality that was previously only available in the interpreter, and sets the stage for husk to begin adding support for R<sup>7</sup>RS.

API changes:

- The Compiler Haskell API has been reorganized to use a Types module, and a new Libraries module has been added to store code for libraries.

Bug fixes:

- Allow the husk compiler to reference variables that are defined later in the program. For example, the following now compiles: `(define (foo) (bar))` `(define (bar) (foo))`
- Fixed `string-set!` to allow an expression to be passed as the character argument.
- Fixed `string-ref` to work when the string argument is passed using a variable.

v3.10
--------

This release includes many important fixes and enhancements to the compiler:

- Significantly reduced the amount of compiled Haskell code generated by huskc, leading to a 40% reduction in the code generated by the compiler test suite. Note any actual percentages will depend upon the scheme code being compiled.
- Added `load` as a compiler special form, to allow code to be loaded from other files and compiled alongside the main program.
- Added optimizations to evaluate primitive expressions at compile time, and to generate more efficient code for functions that are only passed literal arguments.
- Enhanced the compiler to detect undefined variables and report an error at compile time, instead of generating code that will throw a runtime error.
- Fixed an issue where the compiler would not cache macro expansions in its local memory when compiling function definitions. This bug caused problems for explicit renaming macros, as it was possible for the evaluator to evaluate a special form thinking that it was a function, resulting in an "unbound variable" error.
- Modified the test suite to work with both the interpreter and compiler. Note that some test cases are not executed by the compiler because they are not supported (see below).
- SRFI 1 is not supported by the compiler at this time.

v3.9
--------

- Fixed a syntax error that prevented the FFI module from building. Thanks to Ricardo Lanziano for bringing this to my attention!
- Enhanced the compiler to use the name of the source file for the compiled Haskell output instead of hardcoding the file to `_tmp.hs`. For example, `my-file.scm` will compile to `my-file.hs`.
- Removed extraneous quotes when printing a lambda form.

v3.8
--------

This release introduces several performance improvements:

- Macro expansions are now cached, significantly improving performance when repeatedly calling a function containing macros.
- A `Pointer` type was added in version 3.6 as part of the changes to enhance the variable storage model. Unfortunately the initial implementation naively checked for pointers prior to calling into any Haskell function. This release eliminates those inefficiencies by allowing primitive functions to deal with the Pointer type directly, instead of attempting to convert values before passing them to primitive functions.
- Restructured code in the Macro module to eliminate redundant calls to `Data.Map.lookup`.

The example game of life program `examples/game-of-life/life.scm` demonstrates these performance improvements, as it now runs over 4.5 times faster than in the previous release. 

This release also adds the following library from R<sup>7</sup>RS:

- `(scheme r5rs)` - Exposes the full husk R<sup>5</sup>RS environment

And, R<sup>5</sup>RS versions of the scheme libraries have been relocated to underneath `(scheme r5rs)`. Each of these libraries exposes a husk subset of the functions recommended by R<sup>7</sup>RS:

- `(scheme r5rs base)`
- `(scheme r5rs char)`
- `(scheme r5rs complex)`
- `(scheme r5rs cxr)`
- `(scheme r5rs eval)`
- `(scheme r5rs file)`
- `(scheme r5rs inexact)`
- `(scheme r5rs lazy)`
- `(scheme r5rs load)`
- `(scheme r5rs read)`
- `(scheme r5rs write)`

Changes to the Haskell API:

- Introduced a new type of function, `CustFunc`, which is now the recommended way to define your own Haskell functions when using the Haskell API. This type allows you to avoid having to handle Pointer types directly in your Haskell code. If you know what you are doing, though, you can handle Pointer types and avoid the overhead of checking for pointers prior to calling into your function code.
- Moved `runIOThrows` into Core, and removed obsolete functions `trapError` and `extractValue`.

Bug fixes:

- Updated `map` and `for-each` to accept multiple list arguments, per R<sup>5</sup>RS.

v3.7
--------

A major change for this release is the introduction of Scheme libraries using R<sup>7</sup>RS library syntax. For an example of how to use libraries, see `examples/hello-library/hello.scm` in the husk source tree. Note that since R<sup>7</sup>RS is not currently implemented by husk, the library system only has the built-in import `(r5rs base)` to allow you to import the standard husk R5RS environment. Also, please keep in mind this is still a beta feature that is not yet implemented by the compiler.

This release also contains many improvements to the Haskell API:

- Added `r5rsEnv` to the Core module to expose the full environment, including functions loaded from the Scheme standard library.
- Added `getDataFileFullPath` to the Core module to allow third party code to reference Scheme files such as `stdlib.scm` that are installed alongside husk.
- Modified `NumArgs` to optionally require an explicit number of arguments. This helps when writing variable-length functions where specifying a single number of arguments may be misleading.
- Added a new module `Language.Scheme.Util` to contain general purpose utility functions.

Bug fixes:

- Updated the parser to accept floating point numbers that contain only a fractional component, such as `.5`.
- Enhanced numerical comparison operators (`=`, `<`, `<=`, `>`, `>=`) to be able to accept an unlimited number of arguments, per R5RS.

v3.6.3
--------

Added support for R<sup>7</sup>RS bytevectors.

Improved support for using husk as an extension language by adding Haskell function `evalLisp'`. This function evaluates a lisp data structure and returns the `LispVal` or `LispError` result directly:

    evalLisp' :: Env -> LispVal -> IO (ThrowsError LispVal)

This makes it much easier to retrieve results when using husk as an extension language:

    result <- evalLisp' env $ List [Atom "/", Number 1, Number 0]
    case result of
      Left err -> putStrLn $ "Error: " ++ (show err)
      Right val -> putStrLn $ show val

Finally, fixed a bug where setting a variable to refer back to itself would result in an infinite loop. For example, the last line of the following code would cause `huski` to hang:

    (define a '())
    (define b a)
    (define a b)

v3.6.2
--------
This release adds support for nested quasi-quotation forms, which now respect depth level. This was done by replacing the quasi-quotation special form with a macro based on the one from chibi scheme. A nice side-benefit is that by removing the special forms, quasi-quotation now works in the compiler.

Also added support for SRFI 2, `and-let*`. From the SRFI document:

> Like an ordinary AND, an AND-LET\* special form evaluates its arguments - expressions - one after another in order, until the first one that yields #f. Unlike AND, however, a non-#f result of one expression can be bound to a fresh variable and used in the subsequent expressions. AND-LET\* is a cross-breed between LET\* and AND.

And added support for environment specifiers, including the following functions:

- `(interaction-environment)`
- `(current-environment)`
- `(make-environment)`
- `(null-environment version)`
- `(load filename environment-specifier)`
- `(eval expression environment-specifier)`

This release also includes the following bug fixes:

- Fixed a bug where nested explicit renaming macros may not always expand properly.
- Unfortunately, the storage model changes introduced in 3.6 cause problems with hash table literals defined using the `#hash()` syntax. For now, hash table literals have been removed to prevent further problems. This feature may be added back in the future.
- Fixed the code for `require-extension` to allow passing multiple SRFI numbers in the same call. For example: `(require-extension (srfi 1 2))`.
- Improved compiler support by diverting renamed variables back into the enclosing environment. Such variables would previously throw a runtime error when accessed by the compiled program.
- Improved compiler support by loading macros defined using `define-syntax` so they are available at runtime.

v3.6.1
--------
Added support for GHC 7.6.

v3.6
--------
Enhanced the variable storage model to correctly store references to objects. For example, consider the following:

    (define x (list 'a 'b 'c))
    (define y x)
    (set-cdr! x 4)

After executing this code, previous versions of husk assigned `(a b c)` to `y`. With this release, husk now evaluates `y` to the expected value of `(a . 4)`.

The more general problem is that certain data types denote a memory location which may be modified by mutator functions such as `set-cdr!`. This is discussed specifically in section 3.4 <b>Storage Model</b>:

>
> Variables and objects such as pairs, vectors, and strings implicitly denote locations or sequences of locations. A string, for example, denotes as many locations as there are characters in the string. (These locations need not correspond to a full machine word.) A new value may be stored into one of these locations using the string-set! procedure, but the string continues to denote the same locations as before.
> 

Internally husk uses Haskell data types, so the husk model differs slightly from the one in R<sup>5</sup>RS - references are used instead of individual memory locations. This has implications for the `set-car!` and `set-cdr!` special forms, where circular lists and similar low-level optimizations are not possible as Haskell lists are used instead of raw pointers. However, these issues aside, the enhanced storage model is a big step forward to bringing husk's variable support closer in line to that of other Schemes.

v3.5.7
--------

The major change in this release is support for explicit renaming macros. This low-level macro system provides the ability to break macro hygiene, if necessary, and offers a macro system that is similar to `defmacro`.

In addition, all of the character functions from R<sup>5</sup>RS have been implemented.

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
