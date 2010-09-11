(load "skim-unit.scm")

(assert-equal (lambda () (complex? 3+4i)) #t)
(assert-equal (lambda () (complex? 3)) #t)
(assert-equal (lambda () (real? 3)) #t)
;(assert-equal (lambda () (real? -2.5+0.0i)) #t)
;(assert-equal (lambda () (real? #e1e103)) #t)
(assert-equal (lambda () (rational? 6/10)) #t)
(assert-equal (lambda () (rational? 6/3)) #t)
;(assert-equal (lambda () (integer? 3+0i)) #t)
;(assert-equal (lambda () (integer? 3.0)) #t)
;(assert-equal (lambda () (integer? 8/4)) #t)

(unit-test-handler-results)
