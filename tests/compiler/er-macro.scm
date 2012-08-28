(define (assert/equal a b)
 (display "Expected ")
 (display a)
 (display " Got ")
 (display b)
 (newline))

(define-syntax call
  (er-macro-transformer
    (lambda (exp rename compare)
          (cdr exp))))

(assert/equal 
  (call list 1 2 3 4)
  (list 1 2 3 4))

(define-syntax swap! ; Wrong! macro is not hygienic
  (er-macro-transformer
    (lambda (form rename compare?)
      car
      (let ((x (cadr form)) (y (caddr form)))
        `(let ((tmp ,x))
            (set! ,x ,y)
            (set! ,y tmp))))))
(let ((a 'a) (b 'b))
    (assert/equal a 'a)
    (assert/equal b 'b)
    (swap! a b)
    (assert/equal a 'b)
    (assert/equal b 'a))

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
    (assert/equal a 'a)
    (assert/equal b 'b)
    (swap! a b)
    (assert/equal a 'b)
    (assert/equal b 'a)) 'a 'b)


; Test case for two renamings of the same atom
(define x 1)
(define-syntax test
  (er-macro-transformer
    (lambda (form rename compare?)
      `((lambda (x)
         (list 
           ',(rename 'x)
           ',(rename 'x)
           ,(compare? x x)
           ,(compare? x (rename 'x))))
        2))))
(assert/equal 
    (cddr (test))
   '(#t #f))

; Test case for compare
(define-syntax test3
  (er-macro-transformer
    (lambda (form rename compare?)
      (let ((val (cadr form)))
        (if (compare? val (rename 'false))
          `#f
          `#t)))))
(assert/equal (test3 true)  #t)
(assert/equal (test3 false) #f)

