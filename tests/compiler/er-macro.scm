(define-syntax call
  (er-macro-transformer
    (lambda (exp rename compare)
          (cdr exp))))

;(assert/equal 
(write
  (call list 1 2 3 4))
; should be =>  (list 1 2 3 4))


