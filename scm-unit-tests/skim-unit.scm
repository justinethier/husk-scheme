(define (unit-test-handler result) (write result))

(define (assert proc) (unit-test-handler (eqv? #t (proc))))

(define (assert-equal proc value) (unit-test-handler (eqv? (proc) value)))

