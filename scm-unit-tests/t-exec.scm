;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for execution-related forms 
;;
(load "skim-unit.scm")
(unit-test-start "apply")

(assert/equal (apply + (list 3 4)) 7) 

(define compose
    (lambda (f g)
          (lambda args
                  (f (apply g args)))))
(assert/equal ((compose sqrt *) 12 75) 30.0)

(unit-test-handler-results)
