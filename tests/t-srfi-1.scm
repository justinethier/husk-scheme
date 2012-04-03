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
(load "../srfi/srfi-1.scm")

(assert/equal (cons* 1 2 3 4)
             '(1 2 3 . 4))

(assert/equal (cons* 1)
              1)

(unit-test-handler-results)
