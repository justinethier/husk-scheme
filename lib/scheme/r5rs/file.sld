;;; An r5rs equivalent to the r7rs-small File library
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
