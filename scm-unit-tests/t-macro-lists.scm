;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; More test cases for macros, specifically to test macros using
;; improper and proper lists.
;;
(load "skim-unit.scm")
(unit-test-start "macros - improper/proper lists")

; A simple example of a pair in the pattern and transform
(define-syntax pair-test
  (syntax-rules ()
     ((_ (a b . c))
      (quote (a b . c)))))
(assert/equal (pair-test (1 2 . 3))
              '(1 2 . 3))
(assert/equal (pair-test (1 2 3))
              '(1 2 3))
(assert/equal (pair-test (1 2 3 . 4))
              '(1 2 3 . 4))
(assert/equal (pair-test (1 2))
              '(1 2))
(assert/equal (pair-test (1 2 3 4 5 6))
              '(1 2 3 4 5 6))

(define-syntax pair-test-2
  (syntax-rules ()
     ((_ ((a) (b) . (c)))
      (quote ((a) (b) . (c))))))
(assert/equal (pair-test ((1) (2) . (3)))
             '((1) (2) 3))
(assert/equal (pair-test ((1) (2) (3)))
             '((1) (2) (3)))
(assert/equal (pair-test ((1) (2) (3) . (4)))
             '((1) (2) (3) 4))
(assert/equal (pair-test ((1) (2)))
             '((1) (2)))
(assert/equal (pair-test ((1) (2) (3) (4) (5) (6)))
             '((1) (2) (3) (4) (5) (6)))

(define-syntax pair-test-nested
  (syntax-rules ()
     ((_ (a b . c) ...)
      (quote (a b . c) ...))))
(assert/equal (pair-test-nested (1 2 . 3))
             '(1 2 . 3))
(assert/equal (pair-test-nested (1 2 3))
             '(1 2 3))
(assert/equal (pair-test-nested (1 2 3 . 4))
             '(1 2 3 . 4))
(assert/equal (pair-test-nested (1 2))
             '(1 2))
(assert/equal (pair-test-nested (1 2 3 4 5 6))
             '(1 2 3 4 5 6))

; A temporary test to demonstrate what is happening in the other tests below.
; We do not handle the case where a variable is in an nary match in the pattern but is
; used directly in the transform. This should certainly be an error in some cases such
; as this one. But the pair/list ones below are real an absolutely must be handled.
(define-syntax test
  (syntax-rules ()
     ((_ (a b c ...))
      (quote (a b c)))))
(assert/equal (test (1 2)) '(1 2))

; A macro taking a pair in the pattern and transforming into a list
(define-syntax pair-test-00
  (syntax-rules ()
     ((_ (a b . c))
      (quote (a b c)))))
(assert/equal (pair-test-00 (1 2 . 3))
             '(1 2 3))

(assert/equal (pair-test-00 (1 2 3))
             '(1 2 (3)))

(assert/equal (pair-test-00 (1 2 3 4))
             '(1 2 (3 4)))
(assert/equal (pair-test-00 (1 2))
             '(1 2))

(assert/equal (pair-test-00 (1 2 3 4 5 . 6))
             '(1 2 (3 4 5 . 6)))
(assert/equal (pair-test-00 (1 2 3 . 6))
             '(1 2 (3 . 6)))

; Same as previous macro, but packaged using (list) - any real difference here?
(define-syntax pair-test-01
  (syntax-rules ()
     ((_ (a b . c))
      (list (quote (a b c))))))

(assert/equal (pair-test-01 (1 2 . 3))
             '((1 2 3)))

(assert/equal (pair-test-01 (1 2 3))
             '((1 2 (3))))

(assert/equal (pair-test-01 (1 2))
             '((1 2)))
;
; TODO: test with this macro:
;
;(define-syntax pair-test-00
;  (syntax-rules ()
;     ((_ (a b . ((c))))
;      (quote (a b c)))))
(unit-test-handler-results)

