(load "skim-unit.scm")

(assert-equal (lambda () (procedure? car)) #t)
(assert-equal (lambda () (procedure? 1)) #f)
(assert-equal (lambda () (procedure? "procedure")) #f)

;(assert-equal (lambda () (string '())) "")
(assert-equal (lambda () (list->string '(#\a))) "a")
(assert-equal (lambda () (list->string '(#\s #\k #\i #\m))) "skim")
(assert-equal (lambda () (list->string '())) "")

