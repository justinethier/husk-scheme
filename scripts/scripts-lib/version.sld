(define-library (scripts-lib version)
  (export
    get-husk-version)
  (import (scheme base)
          (srfi 2))
  (begin
    (define (get-husk-version)
      (call/cc
        (lambda (return)
          (for-each 
            (lambda (sym)
              (and-let* ((str (symbol->string sym))
                         (len-ok (> (string-length str) 5))
                         (husk-ver-ok (equal? (string-copy str 0 5) "husk-")))
                (return (string-copy str 5))))
            (features)))))))
