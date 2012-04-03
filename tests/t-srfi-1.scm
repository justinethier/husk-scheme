;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for SRFI-1
;;
(unit-test-start "SRFI 1: List Library")

; TODO: (import srfi-1)
; Note - can test these in chicken using: (require-extension srfi-1)

(load "../srfi/srfi-1.scm")

(assert/equal (cons* 1 2 3 4) '(1 2 3 . 4))
(assert/equal (cons* 1) 1)
(assert/equal (make-list 4 'c) '(c c c c))
(assert/equal (list-tabulate 4 values) '(0 1 2 3))
; TODO: (assert/equal (iota 5) '(0 1 2 3 4))
; TODO: (assert/equal (iota 5 0 -0.1) '(0 -0.1 -0.2 -0.3 -0.4))

(unit-test-handler-results)
