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

(define-library (scheme r5rs cxr)
    (export
        caaaar
        caaadr
        caaar
        caadar
        caaddr
        caadr
        cadaar
        cadadr
        cadar
        caddar
        cadddr
        caddr
        cdaaar
        cdaadr
        cdaar
        cdadar
        cdaddr
        cdadr
        cddaar
        cddadr
        cddar
        cdddar
        cddddr
        cdddr
    )
    (import (scheme r5rs)))
