(load "../stdlib.scm")

(define (unit-test-handler expected result) 
  (if (not (eqv? expected result))
    (write (list "Test failed. Expected: " expected " Observed: " result))
    #t))

(define (assert proc) (unit-test-handler #t (proc)))

(define (assert-equal proc value) (unit-test-handler value (proc)))

