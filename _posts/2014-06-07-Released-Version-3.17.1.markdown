--- 
layout: post
title: Released Version 3.17.1
excerpt: This release includes bug fixes and improvements to the core interpreter.
---
# {{ page.title }}

- Added `call-with-port` from R<sup>7</sup>RS.

Refactoring:

- Improved passing of extra arguments within the interpreter by removing the `extraReturnArgs` parameter from `Continuation` and adding it as an extra parameter to the `continueEval` function. That way a new `Continuation` object does not need to be created each time the function is called.
- Reduced size of compiled code by approximately 10%.

Bug Fixes:

- Added error checking to many I/O functions to prevent crashes when using a port that has already been closed.
- Added optional start/end arguments to `string->vector`, `vector->string`, `vector-copy!`, and `string-copy!`.
