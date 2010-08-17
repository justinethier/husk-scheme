; TODO: bug - if the first line of the file is blank, (load) spits out a Prelude.last error!
(load "skim-unit.scm")

(define-syntax my-let 
  (syntax-rules ()
    ((my-let x)
	(begin x))))
(define x "hello, world")

(assert-equal (lambda () (my-let 2)) 2)
(assert-equal (lambda () (my-let x)) "hello, world")
(assert-equal (lambda () (my-let (+ 1 2 3 4 5))) 15)


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

; TODO - get each of these working next, then maybe try the real (my-let):
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

; "Fake" my-let test
(define-syntax my-let
  (syntax-rules ()
    ((_ e1 ...)
    ((lambda () e1 ...)))))
(assert-equal (lambda () (my-let (+ 1 2))) 3)

; The 'real' let:
(assert-equal (lambda () (let ((x 1) (y 2) (z 3)) (+ x y z))) 6)
(assert-equal (lambda () (let ((x 11) (y 22) (z 34)) (+ x y z))) 67)
(assert-equal (lambda () (let ((x (* 1 2 3 4)) (y 22) (z 34)) (+ x y z))) (+ 24 22 34))

(assert-equal (lambda () (let () (let ((x 1)) x))) 1)
(assert-equal (lambda () ((lambda () (let ((x 1)) x)))) 1)

; TODO: named let
;(let loop ((numbers '(3 -2 1 6 -5))
;(let loop ((numbers '(3 2 1 6 5))
;           (nonneg '())
;           (neg '()))
;    (cond ((null? numbers) (list nonneg neg))
;          ((>= (car numbers) 0)
;           (loop (cdr numbers)
;                 (cons (car numbers) nonneg)
;                  neg))
;          ((< (car numbers) 0)
;           (loop (cdr numbers)
;                 nonneg
;                 (cons (car numbers) neg)))))


; TODO:
;(assert-equal (lambda () 
;  (let () (let ((x 1)) x)))
;   1)

(unit-test-handler-results)
