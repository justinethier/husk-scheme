(define-syntax call
  (er-macro-transformer
    (lambda (exp rename compare)
          (cdr exp))))

(write 
  (call list 1 2 3 4))
