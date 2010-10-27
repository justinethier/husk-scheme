(load "skim-unit.scm")

; TODO: will need to make step optional on a per-variable basis
; TODO: need to firm up macro code a bit, mostly on error reporting / crashing side...
(define-syntax do
  (syntax-rules ()
     ((_ ((var init step) ...)
          (test expr ...) 
          command ...)
     (let loop ((var init) ...)
       (if test
         (begin expr ...)
         ((begin command ...)
          (loop step ...)))))))

;(do ((vec (make-vector 5) vec)
;          (i 0 (+ i 1)))
;      ((= i 5) vec)
;        (vector-set! vec i i))
;; ===>  #(0 1 2 3 4)

(let ((x '(1 3 5 7 9)))
    (do ((x x (cdr x))
         (sum 0 (+ sum (car x))))
        ((null? x) sum)))
;; ===>  25

;(assert-equal (lambda () (hash-table-ref ht 1)) 1)
;(unit-test-handler-results)
