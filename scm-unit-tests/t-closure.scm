;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for closures
;; From: http://stackoverflow.com/questions/36636/what-is-a-closure
;;
(load "skim-unit.scm")
(unit-test-start "closures")

(define (make-counter)
    (let ((count 0))
        (lambda ()
            (set! count (+ count 1))
            count)))

(define x (make-counter))

(assert/equal (x) 1)
(assert/equal (x) 2)

(unit-test-handler-results)
