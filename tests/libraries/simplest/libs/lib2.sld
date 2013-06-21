;;;
;;; Justin Ethier
;;; husk scheme
;;;
;;; A sample library
;;;
(define-library (libs lib2)
    (export lib2-hello)
    (import (scheme r5rs write))
    (begin
        (define (lib2-hello)
            (write "Hello from library #2"))))
