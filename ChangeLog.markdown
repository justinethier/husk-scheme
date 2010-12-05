v1.2
----
- Fixed an ugly bug where the underlying functions used to mutate variabiables in (define), (set!), etc implemented dynamic scoping instead of the lexical (static) scoping required by R5RS. [This reference implementation](http://web.mit.edu/kmill/Public/lilscheme.hs) written by Kyle Miller was used as a starting point for this change.

v1.1
----
- Many improvements to quasi-quotation, including support for unquote splicing (,@).

v1.0
----
- Initial release
