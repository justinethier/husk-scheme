(load "skim-unit.scm")

(assert/equal (vector? '#(1 2 3 4 5)) #t)
(assert/equal (vector? '(1 2 3 4 5)) #f)
(assert/equal (make-vector 4 "test") '#("test" "test" "test" "test"))
(assert/equal (vector-length (make-vector 4 "test")) 4)
(assert/equal (vector-length '#()) 0)
(assert/equal (vector-ref '#(1) 0) 1)

(define vec '#(1 2 3 4))
(vector-fill! vec "Num")
(assert/equal (id vec) '#("Num" "Num" "Num" "Num"))

(assert/equal (eqv? '#("Num" "Num" "Num") '#("Num" "Num" "Num")) #t)
(assert/equal (eqv? '#("Num" "Num" "Num") '#("Num" "Num" "Num2")) #f)
(assert/equal (eqv? '#("Num" "Num" "Num") '#("Num" "Num" 2)) #f)
(assert/equal (eqv? '#("1" "2" "3") '#(1 2 3)) #f)
(assert/equal (equal? '#("1" "2" "3") '#(1 2 3)) #t)

; TODO:
;(define foo 1)
;(define bar 2)
;`#(,foo ,bar) should be #(1 2) but is not...


; TODO: test cases for the following forms:
;
;              ("vector", Vector),
;              ("vector-set!", vectorSet),
;              ("vector-fill!", vectorFill),
;              ("vector-list", vectorToList),
;              ("list-vector", listToVector),

(unit-test-handler-results)
