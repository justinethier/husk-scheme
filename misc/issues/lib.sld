(define-library (lib)
  (export foo)
  (import (scheme base))
  (begin
    (define-syntax bar
      (syntax-rules ()
        ((_)
         'baz)))
    (define-syntax foo
      (syntax-rules ()
        ((_)
         (bar))))))
