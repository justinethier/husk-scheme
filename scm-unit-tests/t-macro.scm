; TODO: bug - if the first line of the file is blank, (load) spits out a Prelude.last error!
;
;(define-syntax let
;  (syntax-rules ()
;    ((_ ((x v) ...) e1 e2 ...)
;    ((lambda (x ...) e1 e2 ...) v ...))))
;
;(let ((x 9)) (print x) (print (+ x 9)))
; TODO: "let" tests
;
;(define-syntax let (syntax-rules ()    ((_ x)    (x))))
(define-syntax let (syntax-rules ()    ((let x)    (write x))))
(define x "hello, world")
(let 2)
(let x)
(let (+ 1 2 3 4 5))
