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

;
; TODO: test cases for the following forms:
;
;              ("vector", Vector),
;              ("vector-set!", vectorSet),
;              ("vector-fill!", vectorFill),
;              ("vector-list", vectorToList),
;              ("list-vector", listToVector),

