(import (scheme base) (scheme write))

(define-syntax foo
  (syntax-rules ()
    ((_ (p ... . r))
     'r)))

(display (foo (1 2 3 4)))
