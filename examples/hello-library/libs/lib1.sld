(define-library (libs lib1)
    (export lib1-hello)
    (import (r5rs base)
            (libs lib2))
    (begin
        (define (lib1-hello)
            (write lib2-hello))))
