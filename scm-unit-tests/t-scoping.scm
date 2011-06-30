;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for eval 
;;
(load "skim-unit.scm")
(unit-test-start "lexical scope")

; Lexical scoping must work even for special forms:
(assert/equal (let ((if +)) (if 1 2 3)) 6)
(assert/equal (let ((cond +)) (cond 1 2 3)) 6)
(assert/equal (let ((quote +)) (quote 1 2 3)) 6)
(assert/equal (let ((else +)) (else 1 2 3)) 6)
(assert/equal (let ((set! +)) (set! 1 2 3)) 6)
(assert/equal (let ((begin +)) (begin 1 2 3)) 6)
;(assert/equal (let ((define +)) (define 1 2 3)) 6)
;(assert/equal (let ((lambda +)) (lambda 1 2 3)) 6)

(unit-test-handler-results)
