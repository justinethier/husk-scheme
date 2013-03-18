;;; An r5rs equivalent to the r7rs-small Eval library
(define-library (scheme r5rs eval)
    (export 
        eval
        current-environment
        interaction-environment
        make-environment
        null-environment)
    (import (scheme r5rs)))
