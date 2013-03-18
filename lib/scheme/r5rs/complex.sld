;;; An r5rs equivalent to the r7rs-small Complex library
(define-library (r5rs complex)
    (export
        angle
        imag-part
        magnitude
        make-polar
        make-rectangular
        real-part
    )
    (import (r5rs)))
