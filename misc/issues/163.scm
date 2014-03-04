(import (scheme base)
        (scheme write))

(define-syntax if-identifier
  (syntax-rules ()
    ((_ condition seq alt)
     (let-syntax ((foo (syntax-rules () ((_) seq))))
       (let-syntax ((test (syntax-rules ()
                            ((_ condition) (foo))
                            ((_ x) alt))))
         (test foo))))))

(define-syntax if-member
  (syntax-rules ()
    ((_ p (literals ...) seq alt)
     (let-syntax ((foo (syntax-rules () ((_) seq))))
       (let-syntax ((baz (syntax-rules (literals ...)
                         ((_ literals) (foo)) ...
                         ((_ _) alt))))
       (baz p))))))

(define-syntax foo
  (syntax-rules ()
    ((_ p)
     (if-identifier p
       (if-member p (A B C)
         "A or B or C"
         "Another identifier")
       "Not identifier"))))

(display (foo A))
; should be "A or B or C"
