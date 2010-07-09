(load "skim-unit.scm")

(assert-equal (lambda () (vector? '#(1 2 3 4 5))) #t)
(assert-equal (lambda () (vector? '(1 2 3 4 5))) #f)
(assert-equal (lambda () (make-vector 4 "test")) '#("test" "test" "test" "test"))

; TODO:
;              ("make-vector", makeVector),
;{- TODO:
;              ("vector", Vector),
;              ("vector-length", vectorLength),
;              ("vector-ref", vectorRef),
;              ("vector-set!", vectorSet),
;              ("vector-fill!", vectorFill),
;              ("vector-list", vectorToList),
;              ("list-vector", listToVector),

