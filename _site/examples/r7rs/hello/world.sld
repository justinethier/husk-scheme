; A sample library
(define-library (hello world)
    (export hello)
    (import (scheme base))
    (begin (define hello "hello, world"))) 
