;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases to ensure proper lexical scope
;;
(unit-test-start "lexical scope")

; Lexical scoping must work even for special forms:
(assert/equal (let ((if +)) (if 1 2 3)) 6)
(assert/equal (let ((if +)) (if 1 2 3 4)) 10)
(assert/equal (let ((cond +)) (cond 1 2 3)) 6)
(assert/equal (let ((quote +)) (quote 1)) 1)
(assert/equal (let ((else +)) (else 1 2 3)) 6)
(assert/equal (let ((set! +)) (set! 2)) 2)
(assert/equal (let ((set! +)) (set! 1 2 3)) 6)
(assert/equal (let ((begin +)) (begin 1 2 3)) 6)
(assert/equal (let ((define +)) (define 3)) 3)
(assert/equal (let ((define +)) (define 1 2 3)) 6)
(assert/equal (let ((lambda +)) (lambda 1 2 3)) 6)

(assert/equal (let ((string-set! +)) (string-set! 1 2 3)) 6)
(assert/equal (let ((set-car! +)) (set-car! 1 2 3)) 6)
(assert/equal (let ((set-cdr! +)) (set-cdr! 1 2 3)) 6)
(assert/equal (let ((vector-set! +)) (vector-set! 1 2 3)) 6)
(assert/equal (let ((hash-table-delete! +)) (hash-table-delete! 1 2 3)) 6)
(assert/equal (let ((hash-table-set! +)) (hash-table-set! 1 2 3)) 6)

(define x 1)
(assert/equal (let ((x +)) (x 1 2 3)) 6)
(assert/equal (let () (define x *) (x 1 2 3)) 6)

; Other tests from issue #30

(define-syntax orr 
  (syntax-rules () 
   ((orelse <expr1> <expr2>) 
    (let ((temp <expr1>)) 
         (if temp temp <expr2>)))))

; see Issue #30
(assert/equal
    (let ((temp 4)) (orr #f temp))
    4)

(assert/equal
    (let ((if +)) (orr 1 1))
    1)

(assert/equal
    (let ((if +)) (if (orr 1 1) 10 100))
    111)

; Test case for Issue #52
(let ()
    (define if (lambda (x y z) 'test))
    (assert/equal ((lambda () (if 1 2 3))) 'test)
)

(unit-test-handler-results)
