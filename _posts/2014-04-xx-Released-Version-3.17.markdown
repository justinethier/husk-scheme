--- 
layout: post
title: Released Version 3.17
excerpt: This release adds support for record types, parameter objects, a process-context library, and various bug fixes.
---
# {{ page.title }}

- Added support for [`define-record-type`](http://justinethier.github.io/husk-scheme/manual/node57.html) from R<sup>7</sup>RS and SRFI 9. This syntax allows creation of new disjoint types supporting access to multiple fields.
- Added support for parameter objects from R<sup>7</sup>RS and SRFI 39. See [dynamic bindings](http://justinethier.github.io/husk-scheme/manual/node41.html) in the user manual for more information.
- Added a `(scheme process-context)` library containing the following functions:
     - [`emergency-exit`](http://justinethier.github.io/husk-scheme/manual/node86.html#emergency-exit)
     - [`exit-fail`](http://justinethier.github.io/husk-scheme/manual/node86.html#exit-fail)
     - [`exit-success`](http://justinethier.github.io/husk-scheme/manual/node86.html#exit-success)
     - [`get-environment-variable`](http://justinethier.github.io/husk-scheme/manual/node86.html#get-environment-variable)
     - [`get-environment-variables`](http://justinethier.github.io/husk-scheme/manual/node86.html#get-environment-variables)
     - [`system`](http://justinethier.github.io/husk-scheme/manual/node86.html#system)

Bug Fixes:

- Fixed a macro bug where the last element of a pattern's improper list may not be matched correctly if there is an ellipsis earlier in the list.
- Prevent infinite recursion when evaluating a pointer that contains a pointer to itself.
- Fixed the compiler to add full support for splicing of `begin` definitions.
- Updated `dynamic-wind` to return the value from the `during` thunk instead of the `after` thunk.
