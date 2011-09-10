;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for macros
;;
(load "skim-unit.scm")
(unit-test-start "macros")

(define-syntax my-let 
  (syntax-rules ()
    ((my-let x)
	(begin x))))
(define x "hello, world")

(assert/equal (my-let 2) 2)
(assert/equal (my-let x) "hello, world")
(assert/equal (my-let (+ 1 2 3 4 5)) 15)


(define-syntax test (syntax-rules () ((test 1 ...) (list 1))))
(assert/equal (test) '(1))
(assert/equal (test 1) '(1))
(assert/equal (test 1 1 1 1 1 1 1 1 1 1) '(1))
; FUTURE (unit test framework): this should not pass, since 2 is not in the pattern - (test 1 2)
;                               this test case works, but can't put it here since it halts the program.
;                               would be nice if there was a way to test this...

(define-syntax test (syntax-rules () ((test 1 ... 2) (list 1 2))))
(assert/equal (test 2) '(1 2))
(assert/equal (test 1 2) '(1 2))
(assert/equal (test 1 1 1 1 2) '(1 2))

(define-syntax test (syntax-rules () ((test 1 ... 2 ... 3) (list 1 2 3))))
(assert/equal (test 3) '(1 2 3))
(assert/equal (test 2 3) '(1 2 3))
(assert/equal (test 1 2 3) '(1 2 3))
(assert/equal (test 1 1 1 2 2 2 3) '(1 2 3))

(define-syntax test (syntax-rules () ((test x ...) (list 1))))
(assert/equal (test "hello, world!" (+ 1 2 3) x) '(1))
(assert/equal (test "hello, world!" 1 2 3) '(1))

(define-syntax test (syntax-rules () ((test x ...) (list x ...))))
(assert/equal (test "hello, world!") '("hello, world!"))
(assert/equal (test 3 2 1) '(3 2 1))
(assert/equal (test 'a 'b "c" #\d) '(a b "c" #\d))
;Question: with above macro, what happens when transform is just (list x) - assume an error?

(define-syntax test (syntax-rules () ((_ (1 2) (3 x)) (list x))))
(assert/equal (test (1 2) (3 4)) '(4))

(define-syntax test (syntax-rules () ((_ (1 2) (3 . x)) (list x))))
(assert/equal (test (1 2) (3 . 4)) '(4))

(define-syntax my-let
  (syntax-rules ()
    ((_ e1 ...)
    ((lambda () e1 ...)))))
(assert/equal (my-let (+ 1 2)) 3)

; let
(assert/equal (let ((x 1) (y 2) (z 3)) (+ x y z)) 6)
(assert/equal (let ((x 11) (y 22) (z 34)) (+ x y z)) 67)
(assert/equal (let ((x (* 1 2 3 4)) (y 22) (z 34)) (+ x y z)) (+ 24 22 34))

(assert/equal (let () (let ((x 1)) x)) 1)
(assert/equal ((lambda () (let ((x 1)) x))) 1)

; let*
(assert/equal (let* () 1) 1)
(assert/equal (let* ((x 1)) x) 1)
(assert/equal (let* ((x 1) (y x)) (+ x y)) 2)
(assert/equal (let* ((x 1)
                                (y x)
                                (z (+ x y))) (* x y z)) (* 1 1 2))
; letrec
(assert/equal 
  (letrec ((even?
          (lambda (n)
            (if (zero? n)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (zero? n)
                #f
                (even? (- n 1))))))
   (even? 88))
  #t)

(assert/equal 
  (letrec ((even?
          (lambda (n)
            (if (zero? n)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (zero? n)
                #f
                (even? (- n 1))))))
   (odd? 88))
  #f)

; named let
(assert/equal
  (let loop ((numbers '(3 -2 1 6 -5))
           (nonneg '())
           (neg '()))
    (cond ((null? numbers) (list nonneg neg))
          ((>= (car numbers) 0)
           (loop (cdr numbers)
                 (cons (car numbers) nonneg)
                  neg))
          ((< (car numbers) 0)
           (loop (cdr numbers)
                 nonneg
                 (cons (car numbers) neg)))))
 '((6 1 3) (-5 -2)))

; Allow having a literal identifier specified 0-or-more times
;
(define-syntax my-test
  (syntax-rules (a)
    ((_ a ... b)
     (list '(b)))))
(assert/equal (my-test 2) '((2)))
(assert/equal (my-test a 2) '((2)))
(assert/equal (my-test a a a a a 2) '((2)))


; FUTURE: support, test cases for
; let-syntax and letrec-syntax


; Dotted lists (pairs)
;
; According to the spec, the following is one of the cases where an input form F matches a pattern P:
;
; P is an improper list (P1 P2 ... Pn . Pn+1) and F is a list or improper list of 
; n or more forms that match P1 through Pn, respectively, and whose nth ``cdr'' matches Pn+1;
;
; The following test cases attempt to cover all possible permutations of this case, comparing
; dotted (improper) lists, lists (with Pn+1 missing), and lists (with Pn+1 present).
;
; First test macro, tests that pairs are correctly processed in the pattern
(define-syntax my-pair-test/01
  (syntax-rules ()
     ((_ (var init . step))
      (list (quote (var init step))))))

; Second test macro, tests that pairs are correctly detected in pattern and
; are then transformed correctly as well.
(define-syntax my-pair-test/02
  (syntax-rules ()
     ((_ (var init . step))
      (list (quote (var init . step))))))

; Following test cases for v01 are without the dot in the transform. (IE: (var init step))
; need test cases for both versions of the macro (IE: (var init . step) as well)
(assert/equal
                (my-pair-test/01 (1 2 . 3))
                 '((1 2 3)))
(assert/equal
                (my-pair-test/01 (1 2 3))
                 '((1 2 (3))))
(assert/equal
                (my-pair-test/01 (1 2))
                '((1 2 ())))  
(assert/equal
                (my-pair-test/01 (1 (2 3 4 5) . 4))
                '((1 (2 3 4 5) 4)))

(assert/equal
                (my-pair-test/02 (1 2))
                '((1 2)))  

(define-syntax my-pair-test/03
  (syntax-rules (step)
     ((_ (var init . step))
      (list (quote (var init . step))))))

(assert/equal
                (my-pair-test/02 (1 2 . 3))
                 '((1 2 . 3)))
(assert/equal
                (my-pair-test/02 (1 2 3))
                 '((1 2 3)))

(assert/equal
                (my-pair-test/02 (1 (2 3 4 5) . 4))
                '((1 (2 3 4 5) . 4)))

(assert/equal
                (my-pair-test/03 (1 2 . step))
                 '((1 2 . step)))
(assert/equal
                (my-pair-test/03 (1 (2 3 4 5) . step))
                '((1 (2 3 4 5) . step)))

(define-syntax my-pair-test/04
  (syntax-rules ()
     ((_ (var init . step) ...)
      (quote ((var init . step) ...)))))

(assert/equal
  (my-pair-test/04 (1 2 6) (3 4 5))
  '((1 2 6) (3 4 5)))
(assert/equal
  (my-pair-test/04 (1 2) (3 4 5))
  '((1 2) (3 4 5)))


(define-syntax my-pair-test/05
  (syntax-rules ()
     ((_ (var init) ...)
      (quote (((var init)) ...)))))

(define-syntax my-pair-test/06
  (syntax-rules ()
     ((_ (var . init) ...)
      (quote (((var . init)) ...)))))

(assert/equal
   (my-pair-test/05 (1 2) (4 5) (6 7) (8 9))
  '(((1 2)) ((4 5)) ((6 7)) ((8 9))))

(define-syntax my-do/1
  (syntax-rules ()
     ((_ ((var init . step) ...)
         (test expr ...) 
          command ...)
     (let loop ((var init) ...)
       (if test
         (begin expr ...)
         (begin (begin command ...)
                (quote (((((((loop 
                      (list var . step))))))) ...))))))))

(assert/equal
                (my-do/1 ((vec (make-vector 5) vec)
                     (i 0 (+ i 1)))
                    ((= i 5) vec)
                     (vector-set! vec i i))
 (quote (((((((loop (list vec vec))))))) ((((((loop (list i (+ i 1)))))))))))

(define-syntax my-do/2
  (syntax-rules ()
     ((_ ((var init . step) ...)
         (test expr ...) 
          command ...)
     (let loop ((var init) ...)
       (if test
         (begin expr ...)
         (begin (begin command ...)
                (quote (loop 
                      (list var . step) ...))))))))
(assert/equal
                (my-do/2 ((vec (make-vector 5) vec)
                     (i 0 (+ i 1)))
                    ((= i 5) vec)
                     (vector-set! vec i i))
 (quote (loop (list vec vec) (list i (+ i 1)))))

; Issue #9:
(define-syntax my-pair-test/06
  (syntax-rules ()
     ((_ var . step)
      (list (quote (var . step))))))

(assert/equal (my-pair-test/06 (1 . 3))
             '(((1 . 3))))
(assert/equal (my-pair-test/06 (1 2))
             '(((1 2))))
; WRT above, According to csi, with macro defined as (test):
;
; #;2> (test (1 . 3))
; (((1 . 3)))
; #;3> (test (1 2))
; (((1 2)))


;
; Test cases for vector transforms
;
(define-syntax vector-test
  (syntax-rules ()
    ((_ #(1 2)) 
     (quote #(1 2)))))
(assert/equal (vector-test #(1 2)) '#(1 2))
(define-syntax vector-test2
  (syntax-rules ()
    ((_ #(x)) 
     (quote #(x)))))
(assert/equal (vector-test2 #(3)) '#(3))
(define-syntax vector-test3
  (syntax-rules ()
    ((_ #(x y)) 
     (quote #(x y)))))
(assert/equal (vector-test3 #(4 5)) '#(4 5))
(define-syntax vector-test4
  (syntax-rules ()
    ((_ #(x y ...)) 
     (quote #(x y ...)))))
(assert/equal (vector-test4 #(4 5 6 7 8)) '#(4 5 6 7 8))
(assert/equal (vector-test4 #(4)) '#(4))
(define-syntax vector-test5
  (syntax-rules ()
    ((_ #(x) ...)
     (quote (#(x) ...)))))
(assert/equal (vector-test5 #(1) #(4)) '(#(1) #(4)))
(assert/equal (vector-test5 #(1) #(2) #(4)) '(#(1) #(2) #(4)))
(assert/equal (vector-test5) '())
(assert/equal (vector-test5 #(4)) '(#(4)))
(define-syntax vector-test6
  (syntax-rules ()
    ((_ #(x y ...) ...) 
     (quote (#(x y ...) ...)))))
(assert/equal (vector-test6 #(4)) '(#(4)))
(assert/equal (vector-test6 #(1) #(4)) '(#(1) #(4)))
(assert/equal (vector-test6) '())
(assert/equal (vector-test6 #(1 2 3 4)) '(#(1 2 3 4)))
;
; end vector test cases
;

;
; Test cases to show debugging capabilities
;

; Assert that a template can be quoted, allowing someone debugging a macro the
; ability to see the expansion of that macro
(define-syntax orr-debugging 
  (syntax-rules () 
   ((orelse <expr1> <expr2>) 
    '(let ((temp <expr1>)) 
         (if temp temp <expr2>)))))

(assert/equal (orr-debugging 1 1)
              '(let ((temp 1)) (if temp temp 1)))
(unit-test-handler-results)
