;;; An r5rs equivalent to the r7rs-small Write library
(define-library (r5rs write)
    (export
        display
        write
        ; write-shared
        ; write-simple
    )
    (import (r5rs)))
