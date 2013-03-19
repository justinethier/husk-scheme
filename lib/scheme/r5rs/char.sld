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

(define-library (scheme r5rs char)
    (export
        char-alphabetic?
        char-ci<=?
        char-ci<?
        char-ci=?
        char-ci>=?
        char-ci>?
        ;char-downcase
        ;char-foldcase
        char-lower-case?
        char-numeric?
        ;char-upcase
        char-upper-case?
        char-whitespace?
        ;digit-value
        string-ci<=?
        string-ci<?
        string-ci=?
        string-ci>=?
        string-ci>?
        ;string-downcase
        ;string-foldcase
        ;string-upcase
    )
    (import (scheme r5rs)))
