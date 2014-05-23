
(define x 1)
(define (test-func a b c)
  ;(list a (+ x 2) b c))
  (list a (+ 1 2) b c))

(eval '(write (test-func 1 2 3)))
