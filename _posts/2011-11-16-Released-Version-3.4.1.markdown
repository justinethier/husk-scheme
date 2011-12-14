---
layout: post
title: Released Version 3.4.1 
excerpt: Added experimental support for let-syntax and letrec-syntax...
---
# {{ page.title }}

Added experimental support for let-syntax and letrec-syntax.

Bug fixes:

- Fixed a nasty issue where the macro pattern matching code would not properly reset itself when transcribing a macro containing an ellipsis in the middle of a list. For example: `(a ... b c)`. This caused the expanded macro to be incomplete and/or malformed.
- Fixed a bug that would cause the evaluator to terminate if define-syntax was called within a nested scope. 
