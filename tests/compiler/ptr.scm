
; TODO: need to update huskc so all of this works
; under the compiler

(define (assert/equal a b)
(if (not (eqv? a b))
    (display "ERROR: "))
 (display "Expected ")
 (display a)
 (display " Got ")
 (display b)
 (newline))

(define x '())
(assert/equal (list? x) #t)

;0)
(define x '())
(define y x)
; TODO: edge case - y could be assigned to something else later on.
; when we go redefine x below, the logic needs to check to make sure
; y is still bound to x before assigning y the old x value
(define x 1)
(assert/equal 
   '()
    ((lambda (z) z) y))

;0.1) 
; This demonstrates pointers to pointers.
; need to figure out how to handle this, if we do
; it on the back-end or if we do not allow ptrs-to-ptrs
; in the first place (IE, during definition)
(write (list (list (list))))

;0.2)
(define a '())
(define b a)
(set! a '(1))
(assert/equal '(1) a)
(assert/equal '() b)

;1)
(define x (let () (list 'a 'b 'c)))
(define y x)
; Now y is equal to (a b c)
(set-cdr! x 4)
; Now x is equal to  (a . 4)
; and y should be too, per the spec
(assert/equal '(a . 4) x)
(assert/equal '(a . 4) y)
(set-cdr! y 44)
(assert/equal '(a . 44) x)
(assert/equal '(a . 44) y)
(set-car! y 55)
(assert/equal '(55 . 44) x)
(assert/equal '(55 . 44) y)

;1.1)
(define x "test")
(define y x)
(string-set! y 0 #\a)
(assert/equal x "aest")
(assert/equal y "aest")

(define ht (make-hash-table))
(hash-table-set! ht 1 1)
(assert/equal (hash-table-ref ht 1) 1)

;2)
(define x '( a b c))
(assert/equal '(a b c) x)
(define y x)
(define x 1)
(assert/equal 1 x)
(assert/equal '(a b c) y)


; Attempt to prove that updatePointers needs
; to update the list of pointers, not just the
; first one
(define z1 '())
(define z2 '())
(define z3 '())
(let ((x '(1)))
 (set! z1 x)
 (set! z2 x)
 (set! z3 x)
 (set! x 1)
)
(assert/equal 1 x)
(assert/equal '(1) z1)
(assert/equal '(1) z2)
(assert/equal '(1) z3)
; end updatePointers test

;1.2)
; Does not need to return anything, but should not crash either:
; NOTE: a similar issue affects all of the other special forms.
; presently they each expect a symbol
(set-cdr! '(1 . 2) 3)

