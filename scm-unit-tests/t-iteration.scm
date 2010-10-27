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
         (begin (begin command ...)
          (loop step ...)))))))

; TODO: should not need 'vec' at the end of first line; see R5RS spec
(assert-equal (lambda () 
                (do ((vec (make-vector 5) vec)
                     (i 0 (+ i 1)))
                    ((= i 5) vec)
                     (vector-set! vec i i)))
                #(0 1 2 3 4))

(assert-equal (lambda () 
                (let ((x '(1 3 5 7 9)))
                    (do ((x x (cdr x))
                         (sum 0 (+ sum (car x))))
                        ((null? x) sum))))
                25)

(unit-test-handler-results)
