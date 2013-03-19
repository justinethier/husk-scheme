;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; r5rs equivalent libraries
;;;
;;; This library exposes an R5RS equivalent to 
;;; the corresponding R7RS-small library.
;;;

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
