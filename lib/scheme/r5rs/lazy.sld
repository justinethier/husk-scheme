;;; An r5rs equivalent to the r7rs-small Lazy library
(define-library (scheme r5rs lazy)
    (export
        ;delay
        ;delay-force
        force
        make-promise
        ;promise?
    )
    (import (scheme r5rs)))
