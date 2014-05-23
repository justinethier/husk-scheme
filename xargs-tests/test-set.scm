(define x 1)
x
(set! x 2)
; Compiled code does not work because x is optimized out!
;(write x)
x
