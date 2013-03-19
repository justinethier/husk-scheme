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

(define-library (scheme r5rs file)
    (export
        call-with-input-file
        call-with-output-file
        delete-file
        file-exists?
        ;open-binary-input-file
        ;open-binary-output-file
        open-input-file
        open-output-file
        ;with-input-from-file
        ;with-output-to-file
    )
    (import (scheme r5rs)))
