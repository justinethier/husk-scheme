(define x 1)
x
(set! x 2)
(set! x x)
; Compiled code does not work because x is optimized out!
;(write x)
(write
    ((lambda (x) x) 'from-lambda))
x
