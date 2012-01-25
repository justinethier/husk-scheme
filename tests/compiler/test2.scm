(define make-adder
  (lambda (n)
    (lambda (x) (+ x n))))

((make-adder 1) 2)
