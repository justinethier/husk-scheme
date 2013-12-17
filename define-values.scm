;; TODO: adding this as a temporary file to test define-values - remove prior to release!

 (define-syntax define-values
   (syntax-rules ()
   ((define-values () expr)
   (define dummy
   (call-with-values (lambda () expr)
   (lambda args #f))))
 
   ((define-values (var) expr)
   (define var expr))
 
   ((define-values (var0 var1 ... varn) expr)
   ;(begin
   ((lambda ()
   (define var0
   (call-with-values (lambda () expr)
   list))
   (define var1
   (let ((v (cadr var0)))
   (set-cdr! var0 (cddr var0))
   v)) ...
   (define varn
   (let ((v (cadr var0)))
   (set! var0 (car var0))
   v)))))
 
 ;  ((define-values (var0 var1 ... . varn) expr)
 ;  ;(begin
 ;  ((lambda ()
 ;  (define var0
 ;  (call-with-values (lambda () expr)
 ;  list))
 ;  (define var1
 ;  (let ((v (cadr var0)))
 ;  (set-cdr! var0 (cddr var0))
 ;  v)) ...
 ;  (define varn
 ;  (let ((v (cdr var0)))
 ;  (set! var0 (car var0))
 ;  v)))))
 
   ((define-values var expr)
    (define var
     (call-with-values (lambda () expr)
       list)))))

(define-values (x y) (integer-sqrt 17))
(write (list x y))
    ;==&gt; (4 1)

;:!huski test.scm
;Error: Unexpected error processing data in transformRule, a = var0, var = x

;(write
; (let ()
;  (define-values (x y) (values 1 2))
;    (+ x y))) ;==&gt; 3

