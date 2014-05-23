(define x 1)
(set! x 2)
; Compiled code does not work because x is optimized out!
x
;(write x)
