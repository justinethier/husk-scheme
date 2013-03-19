---
layout: post
title: Released Version 3.8
excerpt: This introduces several performance improvements, as well as updates to Scheme libraries and the Haskell API.
---
# {{ page.title }}

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
