(define (assert/equal a b)
 (display "Expected ")
 (display a)
 (display " Got ")
 (display b)
 (newline))

(define x '())
(assert/equal (list? x) #t)

;;0)
;(define x '())
;(define y x)
;(define x 1)
;(assert/equal 
;   '()
;    ((lambda (z) z) y))
;
;;1)
;(define x (list 'a 'b 'c))
;(define y x)
;; Now y is equal to (a b c)
;(set-cdr! x 4)
;; Now x is equal to  (a . 4)
;; and y should be too, per the spec
;(assert/equal '(a . 4) x)
;(assert/equal '(a . 4) y)
;
;;2)
;(define x '( a b c))
;(assert/equal '(a b c) x)
;(define y x)
;(define x 1)
;(assert/equal 1 x)
;(assert/equal '(a b c) y)
;
