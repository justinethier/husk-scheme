;;; An r5rs equivalent to the r7rs-small Char library
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
