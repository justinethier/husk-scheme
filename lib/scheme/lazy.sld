;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; r5rs lazy library
;;;

(define-library (scheme lazy)
    (export
        delay
        ;delay-force
        force
        make-promise
        ;promise?
    )
    (include "../lazy.scm")
    (import (scheme base)))
