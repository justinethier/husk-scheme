---
layout: post
title: Released Version 3.5.3 
excerpt: This release adds full support for GHC 7.2.2 and 7.4.1... 
---
# {{ page.title }}

This release adds full support for GHC 7.2.2 and 7.4.1, as well as a number of small enhancements.

- Fixed the FFI to work in both GHC 7.2.2 and GHC 7.4.1.
- Implemented SRFI 23, which adds error reporting via the `(error)` function.
- Added support for `let-syntax`, `letrec-syntax`, `set-car!`, `set-cdr!`, `vector-set!`, and `lambda` (pair syntax) to the huskc compiler.
- Added the `--dynamic` option to the compiler to use dynamic Haskell libraries.
- Added the `--extra` option to the compiler to allow passing of arbitrary arguments directly to ghc.
- Fixed huski to allow any redefinition of `let-syntax` and `letrec-syntax`.
