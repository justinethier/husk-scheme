;;;
;;; Justin Ethier
;;; husk scheme
;;;
;;; A sample library
;;;
(define-library (libs simple-write)
    (export sw-hello)
    (import (scheme r5rs write)
;            (libs lib2)
    )
    (begin
        (define (sw-hello)
            (write "hello"))))
        ;(define (internal-func)
        ;    (write lib2-hello))
        ;(define (lib1-hello)
        ;    (internal-func))))
