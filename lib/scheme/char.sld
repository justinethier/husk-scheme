;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; r7rs-small char library
;;;

(define-library (scheme char)
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
    (import (scheme)))
