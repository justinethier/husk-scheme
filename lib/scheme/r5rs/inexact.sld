;;; An r5rs equivalent to the r7rs-small Inexact library
(define-library (scheme r5rs inexact)
    (export
        acos
        asin
        atan
        cos
        exp
        ;finite?
        ;infinite?
        log
        ;nan?
        sin
        sqrt
        tan
    )
    (import (scheme r5rs)))
