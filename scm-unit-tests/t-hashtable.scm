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

(assert/equal (hash-table-exists? ht 1) #t)
(assert/equal (hash-table-exists? ht "test2") #t)
(assert/equal (hash-table-exists? ht "test3") #f)
(assert/equal (hash-table-size ht) 6)

(hash-table-delete! ht 1)
(assert/equal (hash-table-size ht) 5)

(define test-ht (make-hash-table))
(assert/equal (hash-table? ht) #t)
(assert/equal (hash-table? 'ht) #f)
(assert/equal (hash-table? 42) #f)
(hash-table-set! test-ht "test" "testing")
(assert/equal (hash-table-ref test-ht "test") "testing")
(hash-table-merge! ht test-ht)
(hash-table-merge! ht test-ht)
(assert/equal (hash-table-ref ht "test") "testing")

(assert/equal (hash-table->alist ht) 
              '(("test" "testing") ("test2" "testing") (2 2) (3 3) (4 4) (5 5)))
(assert/equal (hash-table-keys ht)
              '("test" "test2" 2 3 4 5))
(assert/equal (hash-table-values ht)
              '("testing" "testing" 2 3 4 5))

(assert/equal  (hash-table-copy ht) ht)

(define test-ht2 (make-hash-table))
(hash-table-merge! ht test-ht2)
(assert/equal (hash-table-ref ht "test") "testing")

(unit-test-handler-results)
