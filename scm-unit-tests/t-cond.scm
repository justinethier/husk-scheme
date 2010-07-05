(load "skim-unit.scm")

(assert-equal (lambda () (cond ((> 3 2) 'greater) ((< 3 2) 'less)))
			  'greater)
(assert-equal (lambda () (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal)))
			  'equal)

; TODO: => special form
;(cond ((assv 'b '((a 1) (b 2))) => cadr)
;                        (else #f))                 ===>  2

