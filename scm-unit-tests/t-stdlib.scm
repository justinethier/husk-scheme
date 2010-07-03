(load "skim-unit.scm")
(load "../stdlib.scm")

(assert-equal (lambda () (map (curry + 2) '(1 2 3 4))) 
			  '(3 4 5 6))

(assert-equal (lambda () (filter even? '(1 2 3 4))) 
			  '(2 4))

