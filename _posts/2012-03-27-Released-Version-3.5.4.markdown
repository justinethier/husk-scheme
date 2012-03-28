---
layout: post
title: Released Version 3.5.4 
excerpt: This release includes an incremental set of improvements...
---
# {{ page.title }}

This release includes an incremental set of improvements:

- Enhanced `huski` and `huskc` to print errors to console when a runtime exception occurs,
and to not print any results to console unless the user calls an I/O function
such as `write` or `display`.
- Added a special form `expand` that can be used to see the result of a macro
expansion, for debugging purposes. For example: `(expand (let ((x 1)) x))` yields `((lambda (x2) x2) 1)`.
- Allow a list to be enclosed by matched brackets as well as parentheses. For example: `(+ 1 2 [+ 3 4])`.
- Internal change - cleaned up code by using Language pragmas instead of explicitly using extentions in the cabal and make files.
