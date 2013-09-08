;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; r7rs complex library
;;;

(define-library (scheme complex)
    (export
        angle
        imag-part
        magnitude
        make-polar
        make-rectangular
        real-part
    )
    (import (scheme)))
