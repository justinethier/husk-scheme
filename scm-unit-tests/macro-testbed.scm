(load "stdlib.scm")

; Problem below is (list ...)
; our code is not smart enough to know that list is not
; a variable that should be transformed, so (guessing) that
; it returns Nil for list and that stops the macro transform...
(define-syntax my-do
  (syntax-rules ()
     ((_ ((var init . step) ...)
         (test expr ...) 
          command ...)
     (let loop ((var init) ...)
       (if test
         (begin expr ...)
         (begin (begin command ...)
                (quote ((loop 
                      (list var  step)) ...))))))))
; Above line is broken when used as a dotted list:
;(list var .  step)) ...))))))))
; This points to a problem with the dotted list logic, where
; a Nil is returned but (apparently?) the code does not stop 
; executing...


;                (quote (loop 
;                      (list var . step) ...))))))))


;                (loop 
;                  (if (null? (cdr (list var . step))) 
;                      (car  (list var . step))
;                      (cadr (list var . step))) ...)))))))
(write
                (my-do ((vec (make-vector 5) vec)
                     (i 0 (+ i 1)))
                    ((= i 5) vec)
                     (vector-set! vec i i)))
