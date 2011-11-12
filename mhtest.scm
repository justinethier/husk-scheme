(let ()
 ((lambda (var-02)

  (let-syntax ((test-template
   (syntax-rules ()
     ((_)
      var-02))))
  
  ((lambda ()
    (write (test-template))
    (write (let ((var-02 1)) (test-template)))
    (write (let ((var-02 2)) (test-template)))
    (define var-02 3)
    (write (test-template))))) 
  1)))
