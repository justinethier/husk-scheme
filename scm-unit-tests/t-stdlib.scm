;
; Unit tests for the skim standard library
;
(load "skim-unit.scm")

(assert-equal (lambda () (map (curry + 2) '(1 2 3 4))) 
			  '(3 4 5 6))

(assert-equal (lambda () (filter even? '(1 2 3 4))) 
			  '(2 4))

(assert-equal (lambda () (zero? 0)) #t)
(assert-equal (lambda () (length '(0 1 2 3))) 4)
(assert-equal (lambda () (member 1 '(1 2 3 4))) 1)


(assert-equal (lambda () (memq 'a '(a b c))) '(a b c))
(assert-equal (lambda () (memq 'b '(a b c))) '(b c))
(assert-equal (lambda () (memq 'a '(b c d))) #f)
(assert-equal (lambda () (memq (list 'a) '(b (a) c))) #f)
(assert-equal (lambda () (member (list 'a) '(b (a) c))) '((a) c))
(assert-equal (lambda () (memv 101 '(100 101 102))) '(101 102))

(assert-equal (lambda () (list-tail '(a b c d e f g) 5)) '(f g))
(assert-equal (lambda () (list-ref '(a b c d) 2)) 'c)

(assert-equal (lambda () (append '(1 2 3 4 5) '(6 7 "eight" "nine"))) 
              '(1 2 3 4 5 6 7 "eight" "nine"))
(unit-test-handler-results)
