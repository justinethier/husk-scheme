(load "skim-unit.scm")

(define ht (make-hash-table))
(hash-table-set! ht 1 1)
(assert/equal (hash-table-ref ht 1) 1)
(hash-table-set! ht 2 2)
(hash-table-set! ht 3 3)
(hash-table-set! ht 4 4)
(hash-table-set! ht 5 5)
(hash-table-set! ht 1 "test")
(hash-table-set! ht "test2" "testing")

(assert/equal (hash-table-ref ht 1) "test")
(assert/equal (hash-table-ref ht "test2") "testing")

; TODO: Many more simple test cases like this, using expressions and every type of primitive

(assert/equal (hash-table-exists? ht 1) #t)
(assert/equal (hash-table-exists? ht "test2") #t)
(assert/equal (hash-table-exists? ht "test3") #f)
(assert/equal (hash-table-size ht) 6)

(hash-table-delete! ht 1)
(assert/equal (hash-table-size ht) 5)

; TODO:
;              ("hash-table?", isHashTbl),
;              ("hash-table->alist", hashTbl2List),
;              ("hash-table-keys", hashTblKeys),
;              ("hash-table-values", hashTblValues),
;              ("hash-table-copy", hashTblCopy),
;
; TODO: many more functions now, cross-reference core code and add more cases...

(define test-ht (make-hash-table))
(hash-table-set! test-ht "test" "testing")
(assert/equal (hash-table-ref test-ht "test") "testing")
(hash-table-merge! ht test-ht)
(hash-table-merge! ht test-ht)
(assert/equal (hash-table-ref ht "test") "testing")

(define test-ht2 (make-hash-table))
(hash-table-merge! ht test-ht2)
(assert/equal (hash-table-ref ht "test") "testing")

(unit-test-handler-results)
