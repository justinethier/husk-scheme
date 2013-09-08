;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; r7rs-small inexact library
;;;

(define-library (scheme inexact)
    (export
        acos
        asin
        atan
        cos
        exp
        ;TODO: finite?
        ;TODO: infinite?
        log
        ;TODO: nan?
        sin
        sqrt
        tan
    )
    (import (scheme)))
