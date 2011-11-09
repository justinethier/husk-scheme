; expressions from Issue 56
; mangles results: (letrec ((z 12)) z (let () (let ((z 1) (y 2)) (+ z y))) z)

(letrec ((z 12)) (let () (let ((z 1) (y 2)) (+ z y))) z)

; husk expansion
; looks like the z parameter is lost somehow
((lambda () (define z 12) ((lambda () ((lambda () ((lambda (z7 y8) (+ z7 y8)) 1 2)))))))
; chicken expansion
(##core#let () (define50 z 12) (let49 () (let () (let ((z 1) (y 2)) (+ z y))) z))

#|
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
|#
