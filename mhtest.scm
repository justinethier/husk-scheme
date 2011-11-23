; A test case demonstrating that divert does not work in the case
; of a nested let-syntax expression:
(define x 1)
(define-syntax test
 (syntax-rules (x)
  ((_)
   x)))

;(let () ;-syntax ()
(let-syntax ()
 ((lambda (var-02)

  (let-syntax ((test-template
   (syntax-rules ()
     ((_)
      var-02))))
  
  ((lambda ()
    (write (test-template))
    (write (let ((var-02 1)) (test-template)))
    (write (let ((var-02 2)) (test-template)))
    (write (let ((var-02 2)) (test)))
    (define var-02 3)
    (write (test-template))))) 
  ) 1))
