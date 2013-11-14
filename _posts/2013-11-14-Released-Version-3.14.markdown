--- 
layout: post
title: Released Version 3.14
excerpt: Another round of enhancements for R<sup>7</sup>RS.
---
# {{ page.title }}

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
