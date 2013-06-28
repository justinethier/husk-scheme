;;;
;;; Justin Ethier
;;; husk scheme
;;;
;;; A sample library
;;;
(define-library (libs lib1)
    (export lib1-hello)
;    (import (scheme r5rs write)
;            (libs lib2))
    (begin
        (define lib1-hello "hello")))
        ;(define (internal-func)
        ;    (write lib2-hello))
        ;(define (lib1-hello)
        ;    (internal-func))))
