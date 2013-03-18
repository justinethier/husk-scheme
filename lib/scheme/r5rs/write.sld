;;; An r5rs equivalent to the r7rs-small Write library
(define-library (scheme r5rs write)
    (export
        display
        write
        ; write-shared
        ; write-simple
    )
    (import (scheme r5rs)))
