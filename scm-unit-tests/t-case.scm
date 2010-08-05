(load "skim-unit.scm")

(assert-equal (lambda () (case (* 2 3) ((6) '(#t)) (else #f))) 
			  '(#t))

(assert-equal (lambda () (case (* 2 3) ((6) '(#t)) (else #f)))
			  '(#t))

(assert-equal (lambda () (case (* 2 3) ((6) 6) (else #f)))
			  6)

(assert-equal (lambda () (case (* 2 3) ((8 9 10 2 3 4 5 1 3 6) #t) (else #f)))
			  #t)

(assert-equal (lambda () (case (* 2 3) ((4) #f) (else '(#t))))
			  '(#t))

(assert-equal (lambda () (case (* 2 3) ((4 5 7 9 4 2 10) #f) (else '(#t))))
			  '(#t))

(assert-equal (lambda () (case (* 2 3) (else #t)))
			  #t)

(unit-test-handler-results)
