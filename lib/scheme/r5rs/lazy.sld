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

(define-library (scheme r5rs lazy)
    (export
        ;delay
        ;delay-force
        force
        make-promise
        ;promise?
    )
    (import (scheme r5rs)))
