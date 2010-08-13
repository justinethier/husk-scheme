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


(define-syntax let 
  (syntax-rules ()
    ((let x)
	(begin x))))
(define x "hello, world")

(assert-equal (lambda () (let 2)) 2)
(assert-equal (lambda () (let x)) "hello, world")
(assert-equal (lambda () (let (+ 1 2 3 4 5))) 15)


(define-syntax test (syntax-rules () ((test 1 ...) (list 1))))

;TODO: form of atom by itself does not work yet, should pass this test - (assert-equal (lambda () (test)) '(1))

(assert-equal (lambda () (test 1)) '(1))
(assert-equal (lambda () (test 1 1 1 1 1 1 1 1 1 1)) '(1))
; TODO: this should not pass, since 2 is not in the pattern - (test 1 2)
; this test case works, but can't put it here since it halts the program.
; would be nice if there was a way to test this...

(define-syntax test (syntax-rules () ((test 1 ... 2) (list 1 2))))
(assert-equal (lambda () (test 2)) '(1 2))
(assert-equal (lambda () (test 1 2)) '(1 2))
(assert-equal (lambda () (test 1 1 1 1 2)) '(1 2))

(define-syntax test (syntax-rules () ((test 1 ... 2 ... 3) (list 1 2 3))))
(assert-equal (lambda () (test 3)) '(1 2 3))
(assert-equal (lambda () (test 2 3)) '(1 2 3))
(assert-equal (lambda () (test 1 2 3)) '(1 2 3))
(assert-equal (lambda () (test 1 1 1 2 2 2 3)) '(1 2 3))

; TODO - get each of these working next, then maybe try the real (let):
(define-syntax test (syntax-rules () ((test x ...) (list 1))))
(assert-equal (lambda () (test "hello, world!" (+ 1 2 3) x)) '(1))
(assert-equal (lambda () (test "hello, world!" 1 2 3)) '(1))

(define-syntax test (syntax-rules () ((test x ...) (list x ...))))
(assert-equal (lambda () (test "hello, world!")) '("hello, world!"))
(assert-equal (lambda () (test 3 2 1)) '(3 2 1))
(assert-equal (lambda () (test 'a 'b "c" #\d)) '(a b "c" #\d))

;TODO: with above macro, what happens when transform is just (list x) - assume an error?

(define-syntax test (syntax-rules () ((_ (1 2) (3 x)) (list x))))
(assert-equal (lambda () (test (1 2) (3 4))) '(4))

(define-syntax test (syntax-rules () ((_ (1 2) (3 . x)) (list x))))
(assert-equal (lambda () (test (1 2) (3 . 4))) '(4))

; "Fake" let test
(define-syntax let
  (syntax-rules ()
    ((_ e1 ...)
    ((lambda () e1 ...)))))
(assert-equal (lambda () (let (+ 1 2))) 3)

; The 'real' let:
(define-syntax let
  (syntax-rules ()
    ((_ ((x v) ...) e1 e2 ...)
    ((lambda (x ...) e1 e2 ...) v ...))))

(assert-equal (lambda () (let ((x 1) (y 2) (z 3)) (+ x y z))) 6)
(assert-equal (lambda () (let ((x 11) (y 22) (z 34)) (+ x y z))) 67)
(assert-equal (lambda () (let ((x (* 1 2 3 4)) (y 22) (z 34)) (+ x y z))) (+ 24 22 34))


(unit-test-handler-results)
