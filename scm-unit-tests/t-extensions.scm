;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for non-standard extentions 
;;
(load "skim-unit.scm")
(unit-test-start "non-standard extensions")

(assert/equal (gensym "test")
  'test1)

(unit-test-handler-results)
