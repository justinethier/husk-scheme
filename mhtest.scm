; Lot of weird things going on here...
;((lambda ()
;(let ()
  (define var-02 1)
  (define-syntax test-template
   (syntax-rules ()
     ((_)
      var-02)))
  
((lambda ()
  (write (test-template))
  (write (let ((var-02 1)) (test-template)))
  (write (let ((var-02 2)) (test-template)))
  (define var-02 3)
  (write (test-template))
))
