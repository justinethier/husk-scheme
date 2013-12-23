--- 
layout: post
title: Released Version 3.15
excerpt: The big change for this release is an online User Manual that documents Husk's Scheme and Haskell APIs, and explains the Scheme language as implemented by Husk.
---
# {{ page.title }}

The big change for this release is an online [User Manual](http://justinethier.github.io/husk-scheme/manual/index.html) based on the R<sup>7</sup>RS, that documents Husk's Scheme and Haskell APIs, and explains the Scheme language as implemented by Husk. This is going to be a work-in-progress but is mostly complete, and can be used immediately as a reference.

In addition, many smaller fixes and enhancements are included:

- Improved library support so that examples no longer require running in a special mode.
- Added missing functions to `(scheme base)`.
- Added support for the `(scheme case-lambda)` and `(scheme r5rs)` libraries.
- Added libraries for SRFI 1, 2, and 69 so that these features are available via `import`. 
  For example: `(import (srfi 1))`.
- Added `exact-integer-sqrt` from R<sup>7</sup>RS, using the Chibi scheme reference implementation.
- Added `let-values` and `let*-values` from R<sup>7</sup>RS.
- Added the following I/O functions:
    `binary-port?`, `close-port`, `input-port-open?`, `open-binary-input-file`,
    `open-binary-output-file`, `output-port-open?`, `read-bytevector`, 
    `textual-port?`, `u8-ready?`, `write-bytevector`.
- Allow character and string comparison predicates (such as `string=?` and `char=?`) to support more than two arguments.
- Fixed `cond-expand` to support `and` / `or` clauses.
- Renamed `char-upper` and `char-lower` to `char-upcase` and `char-downcase` to match the Scheme specs.
