; TODO:
; This is a sample library, delete it once modules are up-and-running
(define-library (hello world)
    (export hello)
;    (import (scheme base))
    (import (scheme))
    (begin2 
        (define (hello h)
            ; TODO: how to get some of these def's in here?
            ; I understand that is what import is for, but how
            ; to bootstrap that to get primitives such as car, 
            ; cdr, etc
            ;
            ; I guess that is what the scheme module is for????
            ;
            (car h))
        (define hello2 "not exported"))) 
