(load "skim-unit.scm")
(assert/equal (apply + (list 3 4)) 7) 

(define compose
    (lambda (f g)
          (lambda args
                  (f (apply g args)))))
(assert/equal ((compose sqrt *) 12 75) 30)

(unit-test-handler-results)
