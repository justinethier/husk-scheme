;;; An r5rs equivalent to the r7rs-small Inexact library
(define-library (r5rs inexact)
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
    (import (r5rs)))
