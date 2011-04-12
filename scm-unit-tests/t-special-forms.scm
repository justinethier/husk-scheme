;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for special forms 
;;
(load "skim-unit.scm")
(unit-test-start "special forms")

(assert/equal (procedure? car) #t)
(assert/equal (procedure? 1) #f)
(assert/equal (procedure? "procedure") #f)

(assert/equal (procedure? load) #t)
(assert/equal (procedure? apply) #t)
(assert/equal (procedure? eval) #t)
(assert/equal (procedure? call-with-current-continuation) #t)

;(assert/equal (string '())) "")
(assert/equal (list->string '(#\a)) "a")
(assert/equal (list->string '(#\s #\k #\i #\m)) "skim")
(assert/equal (list->string '()) "")

(unit-test-handler-results)
