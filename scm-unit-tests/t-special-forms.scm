(load "skim-unit.scm")

(assert-equal (lambda () (procedure? car)) #t)
(assert-equal (lambda () (procedure? 1)) #f)
(assert-equal (lambda () (procedure? "procedure")) #f)
