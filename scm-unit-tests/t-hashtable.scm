(load "skim-unit.scm")

(define ht (make-hash-table))
;(assert-equal (lambda () (case (* 2 3) ((6) '(#t)) (else #f)))			  '(#t))

(unit-test-handler-results)
