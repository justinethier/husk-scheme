; TODO: test cases for delayed evaluation
(load "skim-unit.scm")

(assert/equal (force (delay 1))
			  1)
(define count 0)
(define x 50)
(define p (delay (begin (set! count (+ count 1))
                        (if (> count x)
                            count
                            (force p)))))

(assert/equal (p) 51)

(unit-test-handler-results)

