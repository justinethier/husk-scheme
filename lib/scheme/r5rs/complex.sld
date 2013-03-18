;;; An r5rs equivalent to the r7rs-small Complex library
(define-library (scheme r5rs complex)
    (export
        angle
        imag-part
        magnitude
        make-polar
        make-rectangular
        real-part
    )
    (import (scheme r5rs)))
