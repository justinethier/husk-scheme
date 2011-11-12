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
    (define var-02 3)
    (write (test-template))))) 
  ) 1))
(let ()
 ((lambda (var-12)

  (let-syntax ((test-template
   (syntax-rules ()
     ((_)
      var-12))))
  
  ((lambda ()
    (assert/equal (test-template) 1)
    (assert/equal (let ((var-12 1)) (test-template)) 1)
    (assert/equal (let ((var-12 2)) (test-template)) 1)
    (define var-12 3)
    (assert/equal (test-template) 1)))) 
  ) 1))

(let-syntax ()
 ((lambda (var-12)

  (let-syntax ((test-template
   (syntax-rules ()
     ((_)
      var-12))))
  
  ((lambda ()
    (assert/equal (test-template) 1)
    (assert/equal (let ((var-12 1)) (test-template)) 1)
    (assert/equal (let ((var-12 2)) (test-template)) 1)
    (define var-12 3)
    (assert/equal (test-template) 1)))) 
  ) 1))

