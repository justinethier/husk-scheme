;;;;
;;;; Examples of using er macros. Most of this code is taken from
;;;; the Chicken docs
;;;;

(define-syntax call
  (er-macro-transformer
    (lambda (exp rename compare)
          (cdr exp))))

(write 
  (call list 1 2 3 4))

(define-syntax swap! ; Wrong! macro is not hygienic
  (er-macro-transformer
    (lambda (form rename compare?)
      (let ((x (cadr form)) (y (caddr form)))
        `(let ((tmp ,x))
            (set! ,x ,y)
            (set! ,y tmp))))))

; TODO: demonstrates that the 3.5.6 macro walker 
; is not compatible with er-macro-transformer
;(let ((a 'a) (b 'b))
;    (write `(,a ,b ,tmp))
;    (swap! a b)
;    (write `(,a ,b ,tmp)))
((lambda (a b)
    (write `(,a ,b ))
    (swap! a b)
    (write `(,a ,b ))) 'a 'b)

(define-syntax swap! 
  (er-macro-transformer
    (lambda (form rename compare?)
      (let (
        (x (cadr form)) 
        (y (caddr form))
        (%tmp (rename 'tmp ))
        (%let (rename 'let ))
        (%set! (rename 'set! ))
        )
        `(,%let ((,%tmp ,x))
            (,%set! ,x ,y)
            (,%set! ,y ,%tmp))))))

((lambda (a b)
    (write `(,a ,b ))
    (swap! a b)
    (write `(,a ,b ))) 'a 'b)

; TODO: need a test case for two renamings of
; the same atom. note that the macro must
; be defined at macro definition time, since
; otherwise the atom is just passed through
; unchanged

(define x 1)
(define-syntax test
  (er-macro-transformer
    (lambda (form rename compare?)
      `((lambda (x)
         (write x)
         '1) 
        2))))
(test)
(define-syntax test2
  (er-macro-transformer
    (lambda (form rename compare?)
      `((lambda (x)
         (write ,(rename 'x))
         '1) 
        2))))
(test2)
; TODO: what about expand? should be able
; to expand an er macro
