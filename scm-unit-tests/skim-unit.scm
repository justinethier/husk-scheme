;;; 
;;; husk-scheme
;;; http://github.com/justinethier/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; Minimal unit testing framework
;;;
(load "../stdlib.scm") ; Explicitly loaded here as unit tests typically invoked during dev
                       ; via 'make test' - as such, it is easier to just add this include
                       ; than to work around the fact that building via 'make' does not
                       ; allow huski to know the exact location of 'stdlib.scm'.

(define pass-count 0)
(define fail-count 0)

(define (unit-test-handler expected actual) 
  (if (not (eqv? expected actual))
    (begin (write (list "Test failed; expected value:" expected ", actual value:" actual))
           (set! fail-count (+ fail-count 1)))
    (set! pass-count (+ pass-count 1))))

(define (assert proc) (unit-test-handler #t (proc)))

(define (assert-equal proc value) (unit-test-handler value (proc)))

(define (assert/equal test expected)
     (unit-test-handler expected ((lambda () test))))

(define (unit-test-handler-results)
  (write `("Test Complete" Passed: ,pass-count Failed: ,fail-count)))
