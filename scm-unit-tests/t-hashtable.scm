(load "skim-unit.scm")

(define ht (make-hash-table))
(hash-table-set! ht 1 1)
(assert-equal (lambda () (hash-table-ref ht 1)) 1)

(hash-table-set! ht 1 "test")
(assert-equal (lambda () (hash-table-ref ht 1)) "test")

;(hash-table-set! ht "test2" "testing")
;(assert-equal (lambda () (hash-table-ref ht "test2")) "testing")

(unit-test-handler-results)
