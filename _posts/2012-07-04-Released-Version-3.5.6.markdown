---
layout: post
title: Released Version 3.5.6 
excerpt: This incremental release allows a hash table to be defined directly using the syntax "#hash(alist)"...
---
# {{ page.title }}

This incremental release allows a hash table to be defined directly using the syntax `#hash(alist)`. For example, you may use `#hash()` to create an empty table or `#hash((a 1) (b . 2))` to create a table containing two elements. This syntax is not part of R<sup>5</sup>RS but seems less clumsy than the standard way, and a similar language feature is provided by Racket.

This release also fixes a bug where `integer?` would always return false for negative rational numbers. For example, `(integer? -2/2)` now correctly evaluates to `#t`.

Finally, several important enhancements were made to the compiler:

- Hash tables are now fully supported by the compiler. This includes both parsing of hash table instances as well as the addition of special forms `hash-table-set!` and `hash-table-delete!`.
- Enhanced the compiler to accept `load-ffi` as a special form, so a compiled version of a program does not have to wait for a module to be dynamically loaded. Instead, the module is included at compile time. This offers a nice speed improvement:

  <pre>
  <code>
     $ time huski ffi-cputime.scm 
     Seconds of CPU time spent: 2.756171

     $ time ./ffi-cputime 
     Seconds of CPU time spent: 2.4001e-2
  </code>
  </pre>

