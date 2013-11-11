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
        finite?
        infinite?
        log
        nan?
        sin
        sqrt
        tan
    )
    (import (scheme)))
