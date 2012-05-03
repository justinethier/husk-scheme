---
layout: post
title: Released Version 3.5.5 
excerpt: TODO
---
# {{ page.title }}

TODO: (This release includes an incremental set of improvements:)

TODO: this is a big deal, should not just be a bullet point:
Added support for SRFI-1 (List Library), which can be loaded using `(require-extension (srfi 1))`. Note that linear update functions (such as `map!`, `take!`, etc) and circular lists are not supported at this time.

Same with this one, or not??
- Added a new LispVal type called `Opaque` for Haskell integration, courtesy of Josh Triplett. The Opaque type allows a native Haskell function to package an arbitrary Haskell type for use by other native code called by husk. See `examples/ffi/Opaque.hs` for an example of how to use this feature.


- Implemented `file-exists?`, `delete-file`, `char-ready?`, `rationalize`, `gcd`, and `lcm`.
- Enhanced the parser to read numbers in scientific notation such as `1e3` and `4.2e1`.
- Modified `numerator` and `denominator` to add support for integers and floating point numbers.
- Fixed a bug in `set-cdr!` where an unsimplified list may be output. For example, `(3 . (2 . (1 . ())))` instead of `(3 2 1)`.
- Allow `apply` to receive multiple arguments, as long as the last one is a list.
