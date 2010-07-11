(load "skim-unit.scm")

(assert-equal (lambda () (vector? '#(1 2 3 4 5))) #t)
(assert-equal (lambda () (vector? '(1 2 3 4 5))) #f)
(assert-equal (lambda () (make-vector 4 "test")) '#("test" "test" "test" "test"))
(assert-equal (lambda () (vector-length (make-vector 4 "test"))) 4)
(assert-equal (lambda () (vector-length '#())) 0)
(assert-equal (lambda () (vector-ref '#(1) 0)) 1)

(define vec '#(1 2 3 4))
(vector-fill! vec "Num")
(assert-equal (lambda () (id vec)) '#("Num" "Num" "Num" "Num"))

(assert-equal (lambda () (eqv? '#("Num" "Num" "Num") '#("Num" "Num" "Num"))) #t)
(assert-equal (lambda () (eqv? '#("Num" "Num" "Num") '#("Num" "Num" "Num2"))) #f)
(assert-equal (lambda () (eqv? '#("Num" "Num" "Num") '#("Num" "Num" 2))) #f)
(assert-equal (lambda () (eqv? '#("1" "2" "3") '#(1 2 3))) #f)
(assert-equal (lambda () (equal? '#("1" "2" "3") '#(1 2 3))) #t)

;
; TODO: test cases for the following forms:
;
;              ("vector", Vector),
;              ("vector-set!", vectorSet),
;              ("vector-fill!", vectorFill),
;              ("vector-list", vectorToList),
;              ("list-vector", listToVector),

