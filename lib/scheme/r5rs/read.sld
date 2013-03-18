;;; An r5rs equivalent to the r7rs-small Read library
(define-library (scheme r5rs read)
    (export read)
    (import (scheme r5rs)))
