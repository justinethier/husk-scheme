; TODO: bug - if the first line of the file is blank, (load) spits out a Prelude.last error!
(load "skim-unit.scm")

;
;(define-syntax let
;  (syntax-rules ()
;    ((_ ((x v) ...) e1 e2 ...)
;    ((lambda (x ...) e1 e2 ...) v ...))))
;
;(let ((x 9)) (print x) (print (+ x 9)))
; TODO: "let" tests
;
;(define-syntax let (syntax-rules ()    ((_ x)    (x))))


(define-syntax let (syntax-rules ()    ((let x)    (begin x))))
(define x "hello, world")

(assert-equal (lambda () (let 2)) 2)
(assert-equal (lambda () (let x)) "hello, world")
(assert-equal (lambda () (let (+ 1 2 3 4 5))) 15)


(define-syntax test (syntax-rules () ((test 1 ...) (list 1))))

;(assert-equal (lambda () (test)) '(1))
(assert-equal (lambda () (test 1)) '(1))
;(assert-equal (lambda () (test 1 1 1 1 1 1 1 1 1 1)) '(1))

; TODO:
;(define-syntax test (syntax-rules () ((test 1 ... 2) (list 1 2))))
;(define-syntax test (syntax-rules () ((test 1 ... 2 ... 3) (list 1 2 3))))

(unit-test-handler-results)
