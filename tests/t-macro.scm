;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for macros
;;
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


; let-syntax and letrec-syntax
(assert/equal (let-syntax () 2) 2)
(assert/equal (let ((x 1)) (let-syntax () x)) 1)
(assert/equal (let-syntax () 1 2 3) 3)
(assert/equal (let-syntax ((my-let (syntax-rules () ((my-let x) (begin x))))) (define x "hello, world") (my-let 2)) 2)

; Cases from R5RS
(assert/equal
  (let-syntax ((when (syntax-rules ()
                ((when test stmt1 stmt2 ...)
                 (if test
                    (begin stmt1
                           stmt2 ...))))))
      (let ((if #t))
            (when if (set! if 'now))
                if))
  'now)
(assert/equal
  (let ((x 'outer))
    (let-syntax ((m (syntax-rules () ((m) x))))
      (let ((x 'inner))
        (m))))
  'outer)
(assert/equal
 (letrec-syntax
  ((my-or (syntax-rules ()
            ((my-or) #f)
            ((my-or e) e)
            ((my-or e1 e2 ...)
             (let ((temp e1))
               (if temp
                   temp
                   (my-or e2 ...)))))))
  (let ((x #f)
        (y 7)
        (temp 8)
        (let odd?)
        (if even?))
    (my-or x
           (let temp)
           (if y)
           y)))
 7)
; End R5RS cases

;; Issue #5
;; TODO: presumably this fails because the macro walker does not
;;  presently have knowledge of let-syntax
;(assert/equal
;  (let ((f (lambda (x) (+ x 1))))
;    (let-syntax ((f (syntax-rules ()
;                      ((_ x) x)))
;                 (g (syntax-rules ()
;                      ((_ x) (f x)))))
;      (list (f 1) (g 1))))  
;  '(1 2))
;
;(assert/equal
;  (let ((f (lambda (x) (+ x 1))))
;    (letrec-syntax ((f (syntax-rules ()
;                         ((_ x) x)))
;                    (g (syntax-rules ()
;                         ((_ x) (f x)))))
;      (list (f 1) (g 1))))  
;  '(1 1))

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

;; TODO:
;; commenting this out for now, it will probably be addressed in a later bugfix release.
;; the issue is that when we flag unused pattern vars, we need to record whether they were
;; part of a dotted list, so we have enough information to be able to make this kind of
;; transform:
;;
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

(assert/equal
  (my-pair-test/04 (1 2) (3 4 5) (6 7) (8 9 10))
  '((1 2) (3 4 5) (6 7) (8 9 10)))

(assert/equal
  (my-pair-test/04 (1 2) (3 4 5) (6 7) (8 9 10) (11 12))
  '((1 2) (3 4 5) (6 7) (8 9 10) (11 12)))

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

; TODO: Issue #54:

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


; Issue #43:
(define-syntax my-pair-test/06
  (syntax-rules ()
     ((_ var . step)
      (list (quote (var . step))))))
(assert/equal (my-pair-test/06 (1 . 3))
             '(((1 . 3))))
(assert/equal (my-pair-test/06 (1 2))
             '(((1 2))))

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

; Issue #44:
(define-syntax when
(syntax-rules ()
((when condition . body) (if condition (begin . body) #f))))

(define x -1)
(assert/equal
  (when (negative? x)
;        (newline)
       '("bad number: negative"))
 '("bad number: negative"))

(assert/equal
  (when (negative? 1)
;        (newline)
       '("bad number: negative"))
  #f)

; Example from http://community.schemewiki.org/?scheme-faq-macros
; that proves that "oversized" matches are automatically shortened as needed.
(define-syntax foo (syntax-rules () ((foo (a ...) (b ...)) '((a b) ...))))
(assert/equal (foo (1 2) (3 4 5))
             '((1 3) (2 4)))

; Issue #56
(assert/equal (letrec ((z 12)) 1 2 z) 12)
(assert/equal (letrec ((z 12)) (let () (let ((z 1) (y 2)) (+ z y))) z) 12)
; End #56

; From:
; http://groups.google.com/group/comp.lang.scheme/msg/3e2d267c8f0ef180?pli=1
(assert/equal 
    (let ((f -)) (let f ((n (f 1))) n))
    -1)

(assert/equal
    (letrec* ((p
               (lambda (x)
                 (+ 1 (q (- x 1)))))
              (q
               (lambda (y)
                (if (zero? y)
                    0
                    (+ 1 (p (- y 1))))))
              (x (p 5))
              (y x))
        y)
    5)

(assert/equal 
    (let-values (((root rem) (exact-integer-sqrt 32)))
        (* root rem))
    35)
(assert/equal 
    (let ((a 'a) (b 'b) (x 'x) (y 'y))
        (let*-values (((a b) (values x y))
                      ((x y) (values a b)))
            (list a b x y))) 
   '(x y x y))

; Issue #163
(define-syntax if-identifier
  (syntax-rules ()
    ((_ condition seq alt)
     (let-syntax ((foo (syntax-rules () ((_) seq))))
       (let-syntax ((test (syntax-rules ()
                            ((_ condition) (foo))
                            ((_ x) alt))))
         (test foo))))))

(define-syntax if-member
  (syntax-rules ()
    ((_ p (literals ...) seq alt)
     (let-syntax ((foo (syntax-rules () ((_) seq))))
       (let-syntax ((baz (syntax-rules (literals ...)
                         ((_ literals) (foo)) ...
                         ((_ _) alt))))
       (baz p))))))

(define-syntax foo
  (syntax-rules ()
    ((_ p)
     (if-identifier p
       (if-member p (A B C)
         "A or B or C"
         "Another identifier")
       "Not identifier"))))
(assert/equal (foo A) "A or B or C")
; END 163

(import (162))
(assert/equal (foo) 'baz)

; Issue #166
(define-syntax foo
  (syntax-rules ()
    ((_ (p ... . r))
     'r)))

(assert/equal (foo (1 2 3 4)) '())
; End 166

(unit-test-handler-results)
