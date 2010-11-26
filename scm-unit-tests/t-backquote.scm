(load "skim-unit.scm")

(assert-equal (lambda () (`(list ,(+ 1 2) 4)))
			  '(3 4))

(assert-equal (lambda () ((let ((name 'a)) `(list ,name ',name))))
              (list a (quote a)))

(assert-equal (lambda () (`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)))
				(a 3 4 5 6 b))

(assert-equal (lambda () (`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))))
                 ((foo 7) . cons))

; TODO: needs vector support
;(assert-equal (lambda () (
;				`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)))
;				#(10 5 2 4 3 8))

; TODO:
; `(1 2 . ,(list 3 4))
; csi returns - (1 2 3 4)

; TODO: more test cases from:
; http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.6
(unit-test-handler-results)

