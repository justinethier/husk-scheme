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
(let ((a 'a) (b 'b))
    (write `(,a ,b))
    (swap! a b)
    (write `(,a ,b)))
;((lambda (a b)
;    (write `(,a ,b ))
;    (swap! a b)
;    (write `(,a ,b ))) 'a 'b)

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

(define x 1)
(define-syntax test
  (er-macro-transformer
    (lambda (form rename compare?)
      `((lambda (x)
         (write x)
         '1) 
        2))))
(test)

; Test case for two renamings of the same atom
(define-syntax test2
  (er-macro-transformer
    (lambda (form rename compare?)
      `((lambda (x)
         (write ,(rename 'x))
         (write ',(rename 'x))
         (write ',(rename 'x))
         (write ,(compare? x x))
         '1) 
        2))))
(test2)

; Test case for compare
(define-syntax test3
  (er-macro-transformer
    (lambda (form rename compare?)
      (let ((val (cadr form)))
        (if (compare? val (rename 'false))
          `#f
          `#t)))))
(write (test3 true))
(write (test3 false))
; TODO: what about expand? should be able
; to expand an er macro
