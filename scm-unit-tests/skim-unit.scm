(load "../stdlib.scm")

(define (unit-test-handler expected actual) 
  (if (not (eqv? expected actual))
    (write (list "Test failed; expected value: " expected ", actual value: " actual))
    #t))

(define (assert proc) (unit-test-handler #t (proc)))

(define (assert-equal proc value) (unit-test-handler value (proc)))

