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

(assert/equal (let ((if +)) (if 1 2 3)) 
              6)

(unit-test-handler-results)
