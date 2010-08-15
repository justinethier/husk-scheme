; TODO: test cases for delayed evaluation
(load "skim-unit.scm")

(assert-equal (lambda () (force (delay 1)))
			  1)
(define count 0)
(define x 0)
(define p (delay (begin (set! count (+ count 1))
                        (if (> count x)
                            count
                            (force p)))))

(unit-test-handler-results)

