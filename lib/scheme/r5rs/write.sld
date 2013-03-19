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

(define-library (scheme r5rs write)
    (export
        display
        write
        ; write-shared
        ; write-simple
    )
    (import (scheme r5rs)))
