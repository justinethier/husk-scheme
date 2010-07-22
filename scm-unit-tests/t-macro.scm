(define-syntax let
  (syntax-rules ()
    ((_ ((x v) ...) e1 e2 ...)
    ((lambda (x ...) e1 e2 ...) v ...))))

; TODO: "let" tests
