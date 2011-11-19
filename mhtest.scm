(write (let ((x "outer"))
  (let-syntax ((m (syntax-rules () ((m) x))))
    (let ((x "inner"))
    (m)))))

; Testing ability of a nested macro def to use a renamed var
(define var-02 222)
(let ()
  (set! var-02 1)
; ((lambda (var-02)

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
)
; 1))

#|
; Lot of weird things going on here...
((lambda ()
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
))
|#
