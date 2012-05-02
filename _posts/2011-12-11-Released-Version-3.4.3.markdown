---
layout: post
title: Released Version 3.4.3 
excerpt: Fixed `let-syntax` and `letrec-syntax` to prevent conflicts between...
---
# {{ page.title }}

- Fixed `let-syntax` and `letrec-syntax` to prevent conflicts between identifiers of the same name in the outer scope and within the macro body.
- Per R5RS, `(if <test> <consequent>)` is supposed to evaluate `<consequent>` for any value of `<test>` except `#f`. However, husk was only allowing `<test>` to pass if it was equal to `#t`. This has been fixed.
- Modified `(append)` to accept an arbitrary number of arguments, per R<sup>5</sup>RS.
- Replaced the macro for `case` with the one from R<sup>5</sup>RS.
