---
layout: post
title: Released Version 3.6.2
excerpt: This release adds many new features including nested quasi-quotation forms, and-let*, and support for environment specifiers as well as many bug fixes.
---
# {{ page.title }}

This release adds support for nested quasi-quotation forms, which now respect depth level. This was done by replacing the quasi-quotation special form with a macro based on the one from chibi scheme. A nice side-benefit is that by removing the special forms, quasi-quotation now works in the compiler.

SRFI 2 (`and-let*`) support is added as well. From the SRFI document:

> Like an ordinary AND, an AND-LET\* special form evaluates its arguments - expressions - one after another in order, until the first one that yields #f. Unlike AND, however, a non-#f result of one expression can be bound to a fresh variable and used in the subsequent expressions. AND-LET\* is a cross-breed between LET\* and AND.

And this release also adds support for environment specifiers, including the following functions:

- `(interaction-environment)`
- `(current-environment)`
- `(make-environment)`
- `(null-environment version)`
- `(load filename environment-specifier)`
- `(eval expression environment-specifier)`

This release also includes the following bug fixes:

- Fixed a bug where nested explicit renaming macros may not always expand properly.
- Unfortunately, the storage model changes introduced in 3.6 cause problems with hash table literals defined using the `#hash()` syntax. For now, hash table literals have been removed to prevent further problems. This feature may be added back in the future.
- Fixed the code for `require-extension` to allow passing multiple SRFI numbers in the same call. For example: `(require-extension (srfi 1 2))`.
- Improved compiler support by diverting renamed variables back into the enclosing environment. Such variables would previously throw a runtime error when accessed by the compiled program.
- Improved compiler support by loading macros defined using `define-syntax` so they are available at runtime.
