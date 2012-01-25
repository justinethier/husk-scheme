;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for non-standard extentions 
;;
(unit-test-start "non-standard extensions")

(assert/equal (gensym "test")
  'test2)

(unit-test-handler-results)
