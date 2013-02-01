; A modified version of hello that works
; with the current in-progress version of husk modules
(define-library (hello world-debug)
    (export hello)
    (import (scheme base))
    (begin2 (define hello "hello, world"))) 
    ;(import (scheme base))
    ;(begin (define hello "hello, world"))) 
