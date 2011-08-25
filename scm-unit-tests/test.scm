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

; TODO:
; Temporarily added vector tests from the regular macro suite, to make sure
; they pass, as a baseline. Once these pass, remove them and add some nested
; test cases that use vectors
#|
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
|#
(define-syntax vector-test5
  (syntax-rules ()
    ((_ #(x) ...)
     (quote (#(x) ...)))))
;(assert/equal (vector-test5 #(1) #(4)) '(#(1) #(4)))
;(assert/equal (vector-test5 #(1) #(2) #(4)) '(#(1) #(2) #(4)))
(assert/equal (vector-test5) '())
;(assert/equal (vector-test5 #(4)) '(#(4)))
;(define-syntax vector-test6
;  (syntax-rules ()
;    ((_ #(x y ...) ...) 
;     (quote (#(x y ...) ...)))))
;(assert/equal (vector-test6 #(4)) '(#(4)))
;(assert/equal (vector-test6 #(1) #(4)) '(#(1) #(4)))
;(assert/equal (vector-test6) '())
;(assert/equal (vector-test6 #(1 2 3 4)) '(#(1 2 3 4)))



#|
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
|#
(unit-test-handler-results)
