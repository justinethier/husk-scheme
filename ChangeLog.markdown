v2.0
----
- Full implementation of continuations via call/cc. This involved reworking
eval to thread a continuation through all versions of this function.

v1.3
----
- Added limited support for continuations, by adding the call/cc keyword and first-class continuations. This is still a work in progress, and only a subset of continuations are supported - continuations can only be used as escape procedures from a function (along the lines of a 'return' call).

v1.2
----
- Fixed an ugly bug where the underlying functions used to mutate variabiables in (define), (set!), etc implemented dynamic scoping instead of the lexical (static) scoping required by R5RS. [This reference implementation](http://web.mit.edu/kmill/Public/lilscheme.hs) written by Kyle Miller was used as a starting point for this change.

v1.1
----
- Many improvements to quasi-quotation, including support for unquote splicing (,@).

v1.0
----
- Initial release
