;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for delay-related built in forms 
;;
(unit-test-start "delayed evaluation")

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

