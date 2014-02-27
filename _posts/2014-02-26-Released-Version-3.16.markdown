--- 
layout: post
title: Released Version 3.16
excerpt: This release improves R<sup>7</sup>RS library support, adds support for in-memory I/O buffers, and adds a new command line option to huski.
---
# {{ page.title }}

Improved import of libraries. Husk now detects cyclic dependencies and throws an error instead of going into an infinite loop. Also, each library is only evaluated once during the import process.

`begin` now has the ability to evaluate contained expressions and definitions as if the enclosing `begin` were not present, per R<sup>7</sup>RS. For example:

        huski> x
        Getting an unbound variable: x
        huski> (begin (define x 28) x)
        28
        huski> x
        28

Added the following R<sup>7</sup>RS I/O functions: 

- `get-output-bytevector`
- `get-output-string`
- `open-input-bytevector`
- `open-input-string`
- `open-output-bytevector`
- `open-output-string`
- `read-string`
- `write-string`

Added an `-i` command line option to `huski`. This option will start the interactive REPL after a file specified on the command line is executed, and has no effect if no file is specified.

The Haskell API's `Port` data type has been extended to include an optional in-memory buffer: `Port Handle (Maybe Knob)`. This change is isolated in husk, but if your code uses any `Port` constructors, you would need to change them, EG: `Port _ Nothing`.

