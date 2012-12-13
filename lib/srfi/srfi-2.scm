; TODO: this is untested!
(define-syntax and-let*
  (syntax-rules ()
    ((and-let* () body ...)
     (begin body ...))
    ((and-let* ((var expr) . rest) . body)
     (let ((var expr))
       (and var (and-let* rest . body))))
    ((and-let* ((expr) . rest) . body)
     (let ((tmp expr))
       (and tmp (and-let* rest . body))))))


; Tests from the SRFI proposal:
; linked from http://srfi.schemers.org/srfi-2/srfi-2.html

; No claws
(expect  '(and-let* () 1) 1)
(expect  '(and-let* () 1 2) 2)
(expect  '(and-let* () ) #t)

(must-be-a-syntax-error '(and-let* #f #t) )
(must-be-a-syntax-error '(and-let* #f) )

; One claw, no body
(expect '(let ((x #f)) (and-let* (x))) #f)
(expect '(let ((x 1)) (and-let* (x))) 1)
(expect '(let ((x 1)) (and-let* ( (x) ))) 1)
(expect '(let ((x 1)) (and-let* ( ((+ x 1)) ))) 2)
(expect '(and-let* ((x #f)) ) #f)
(expect '(and-let* ((x 1)) ) 1)
(must-be-a-syntax-error '(and-let* ( #f (x 1))) )

; two claws, no body
(expect '(and-let* ( (#f) (x 1)) ) #f)
(must-be-a-syntax-error '(and-let* (2 (x 1))) )
(expect '(and-let* ( (2) (x 1)) ) 1)
(expect '(and-let* ( (x 1) (2)) ) 2)
(expect '(and-let* ( (x 1) x) ) 1)
(expect '(and-let* ( (x 1) (x)) ) 1)

; two claws, body
(expect '(let ((x #f)) (and-let* (x) x)) #f)
(expect '(let ((x "")) (and-let* (x) x)) "")
(expect '(let ((x "")) (and-let* (x)  )) "")
(expect '(let ((x 1)) (and-let* (x) (+ x 1))) 2)
(expect '(let ((x #f)) (and-let* (x) (+ x 1))) #f)
(expect '(let ((x 1)) (and-let* (((positive? x))) (+ x 1))) 2)
(expect '(let ((x 1)) (and-let* (((positive? x))) )) #t)
(expect '(let ((x 0)) (and-let* (((positive? x))) (+ x 1))) #f)
(expect '(let ((x 1)) (and-let* (((positive? x)) (x (+ x 1))) (+ x 1)))  3)
(expect
 '(let ((x 1)) (and-let* (((positive? x)) (x (+ x 1)) (x (+ x 1))) (+ x 1)))
  4
  )

(expect '(let ((x 1)) (and-let* (x ((positive? x))) (+ x 1))) 2)
(expect '(let ((x 1)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))) 2)
(expect '(let ((x 0)) (and-let* (x ((positive? x))) (+ x 1))) #f)
(expect '(let ((x #f)) (and-let* (x ((positive? x))) (+ x 1))) #f)
(expect '(let ((x #f)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))) #f)

(expect  '(let ((x 1)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(expect  '(let ((x 0)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(expect  '(let ((x #f)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
(expect  '(let ((x 3)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) (/ 3 2))
