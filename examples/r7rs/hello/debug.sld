(define-library (hello debug)
    (export (rename let let**)  hello2 hello)
    (import (scheme base))
    (begin
        (define (hello h)
            ; TODO: how to get some of these def's in here?
            ; I understand that is what import is for, but how
            ; to bootstrap that to get primitives such as car, 
            ; cdr, etc
            ;
            ; I guess that is what the scheme module is for????
            ;
            call/cc
            (write call/cc)
            (write (car h)))
        (define hello2 "not exported"))) 
