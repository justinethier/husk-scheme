(load "skim-unit.scm")

(assert/equal (procedure? car) #t)
(assert/equal (procedure? 1) #f)
(assert/equal (procedure? "procedure") #f)

;(assert/equal (string '())) "")
(assert/equal (list->string '(#\a)) "a")
(assert/equal (list->string '(#\s #\k #\i #\m)) "skim")
(assert/equal (list->string '()) "")

(unit-test-handler-results)
