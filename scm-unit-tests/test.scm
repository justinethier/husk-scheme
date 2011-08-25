;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; More test cases for macros, used to test nested matching
;;
(load "skim-unit.scm")
(unit-test-start "macros")

(define-syntax nesting-test-simple
  (syntax-rules ()
    ((test a b ...)
     (quote (a b ...)))))
(assert/equal (nesting-test-simple 1 2 5 6 7 8 9 10)
             '(1 2 5 6 7 8 9 10))

; This macro demonstrates multi-level nesting of macros
; It actually crashes v3.2 husk because that version does not properly
; handle nesting
(define-syntax nesting-test
  (syntax-rules ()
    ((test a b (c d (e f ...) ...) ...)
     '(a b ((e f ...) ...) ...))))
(assert/equal (nesting-test 1 2 )
             '(1 2))
(assert/equal (nesting-test 1 2 (3 4 (5 6 7 8 9 10)))
             '(1 2 ((5 6 7 8 9 10))))

(unit-test-handler-results)
