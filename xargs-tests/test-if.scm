(if #t #t #f)
(if #f #t #f)

(define x 'test)
(if x x x)

(write
 (if (zero? 1) 
  ((lambda (x) (+ x (+) (*))) 1)
  ((lambda (x) (+ x (+) (*))) 2)))

