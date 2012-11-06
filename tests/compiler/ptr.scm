
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


; Trying to figure out exactly what updatePointers
; needs to do... note that if a is re-bound, 
; b and c still contain the original value
(define a '(1 2))
(define b a)
(define b a)
(define b a)
(define c a)
(set! a '(1))
(set-car! c 4)
(assert/equal '(1) a)
(assert/equal '(4 2) b)
(assert/equal '(4 2) c)

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




; TODO: these are the other tests from the design doc
;4)
; Closures.
; need to be able to handle closures. something like:

(define a '(1 2))
(define afunc (let ((b a)) (lambda () b)))
(assert/equal '(1 2) (afunc))
(set-car! a 3)
(assert/equal '(3 2) (afunc))
(set! a 3)
(assert/equal 3 a)
; should be previous value from above, NOT 3
(assert/equal '(3 2) (afunc))

; TODO:
;;5)
;; Both expressions return #t even though per spec each should be #f since 
;; each a points to a different memory location.
;(eqv? (list 'a) '(a)) 
;(eq? (list 'a) '(a))


;6)
(define a (lambda (vec) (vector-set! vec 0 #t)))
(let ((vec (make-vector 4 #f)))
  (a vec)
  (assert/equal #(#t #f #f #f) vec))


;7)
;Following cases needs to work, both with or without
; the extra function scope
  (define vec (vector 0 1 2 3 4))
  (vector-fill! vec 5)
  (assert/equal '#(5 5 5 5 5) vec)

  ((lambda ()
    (define vec (vector 0 1 2 3 4))
    (vector-fill! vec 5)
    (assert/equal '#(5 5 5 5 5) vec)))

; using this definition for vector-fill!
; TODO: replace stdlib macro with this definition
(define (vector-fill! vec fill)
 (let loop ((n (vector-length vec)))
   (if (> n 0)
    (begin
      (vector-set! vec (- n 1) fill)
      (loop (- n 1))))))

;8)
;also:
(define a #f)
(define (test v)
  (vector-set! v 0 #t)
  (set! a #t))
(let ((vec '#(0 1 2 3)))
  (test vec)
  (assert/equal '#(#t 1 2 3) vec)
  (assert/equal #t a))

; theory (TBD: this was written a long time ago, does it
;  still apply??):
; what is going in here is that there is no reason to
; pass vec to the function's env, so when vector-set
; searches for vec it never finds it. hence when we
; write vec later on, it retains the original value.
; in order for this to work we would need to pass an
; env of use to the (test) function so that we can
; update any bindings there if necessary.
