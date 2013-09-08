;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; r5rs write library
;;;

(define-library (scheme write)
    (export
        display
        write
        ; write-shared
        ; write-simple
    )
    (import (scheme)))
