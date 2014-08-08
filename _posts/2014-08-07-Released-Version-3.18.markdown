--- 
layout: post
title: Released Version 3.18
excerpt: This release contains bug fixes to many components including syntax-rules macros, for-each, and the Haskell API.
---
# {{ page.title }}

- Added [`exit`](http://justinethier.github.io/husk-scheme/manual/node86.html#exit) from R<sup>7</sup>RS.
- Added support for [SRFI 28 - Basic Format Strings](http://srfi.schemers.org/srfi-28/srfi-28.html).

Bug Fixes:

- Fixed bugs with `syntax-rules` where:
    - A literal identifier may not have been matched in a sub-macro if macro hygiene renamed the input.
    - The environment of macro definition may be overwritten during expansion of a `syntax-rules` macro contained in another macro. This could cause macros defined in a library - but not exported from the library - to incorrectly fail to expand because they are not in scope.
- `for-each` no longer throws an error when an empty list is received.
- In compiled code, the `let-syntax` and `letrec-syntax` forms are now available to `eval` at runtime.
- Added several missing I/O functions to the export list of the `(scheme base)` library.
- bholst added `Unpacker` to the exports from `Language.Scheme.Primitives`, as it is required by `unpackEquals`.
- bholst fixed many comments in the Haddock documentation.
