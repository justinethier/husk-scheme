(load "skim-unit.scm")
(load "../stdlib.scm")

(assert-equal (lambda () (map (curry + 2) '(1 2 3 4))) 
			  '(3 4 5 6))

(assert-equal (lambda () (filter even? '(1 2 3 4))) 
			  '(2 4))

(assert-equal (lambda () (zero? 0)) #t)
(assert-equal (lambda () (length '(0 1 2 3))) 4)
(assert-equal (lambda () (member 1 '(1 2 3 4))) 1)

(assert-equal (lambda () (list-tail '(a b c d e f g) 5)) '(f g))
(assert-equal (lambda () (list-ref '(a b c d) 2)) 'c)

(assert-equal (lambda () (append '(1 2 3 4 5) '(6 7 "eight" "nine"))) 
              '(1 2 3 4 5 6 7 "eight" "nine"))
