--- 
layout: post
title: Released Version 3.11
excerpt: The major change in this release is support for library syntax in the compiler, which sets the stage to begin adding support for R<sup>7</sup>RS.
---
# {{ page.title }}

The major change in this release is support for R<sup>7</sup>RS style library syntax in the compiler. This enables functionality that was previously only available in the interpreter, and sets the stage for husk to begin adding support for R<sup>7</sup>RS.

API changes:

- The Compiler Haskell API has been reorganized to use a `Types` module, and a new `Libraries` module has been added to store code for libraries.

Bug fixes:

- Allow the husk compiler to reference variables that are defined later in the program. For example, the following now compiles: `(define (foo) (bar))` `(define (bar) (foo))`
- Fixed `string-set!` to allow an expression to be passed as the character argument.
- Fixed `string-ref` to work when the string argument is passed using a variable.

