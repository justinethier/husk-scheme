;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; r7rs file library
;;;

(define-library (scheme file)
    (export
        call-with-input-file
        call-with-output-file
        delete-file
        file-exists?
        ;TODO: open-binary-input-file
        ;TODO: open-binary-output-file
        open-input-file
        open-output-file
        ;TODO: with-input-from-file
        ;TODO: with-output-to-file
    )
    (import (scheme)))
