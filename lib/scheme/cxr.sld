;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; The cxr library from r7rs
;;;

(define-library (scheme cxr)
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
        cdddr)
    (include "../cxr.scm")
    (import (scheme base)))
