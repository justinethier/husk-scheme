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

(unit-test-handler-results)

