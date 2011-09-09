;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for do form
;;
(load "skim-unit.scm")
(unit-test-start "iteration")

(assert/equal 
                (do ((vec (make-vector 5) vec)
                     (i 0 (+ i 1)))
                    ((= i 5) vec)
                     (vector-set! vec i i))
                '#(0 1 2 3 4))

(assert/equal #t #f) ;; Adding this as a reminder for below
#|
TODO: this case is going into an infinite loop. However, it seems that the
issue is not with the macro code. Or at least, if it is the problem is within
named let or let*. Anyway, I am commenting it out for now to focus on getting the
rest of the code working after the latest macro changes. Once this branch is in
a better state I will revisit this issue...

; Per spec, <step> does not have to be specified
(assert/equal 
                (do ((vec (make-vector 5))
                     (i 0 (+ i 1)))
                    ((= i 5) vec)
                     (vector-set! vec i i))
                '#(0 1 2 3 4))
|#
(assert/equal 
                (let ((x '(1 3 5 7 9)))
                    (do ((x x (cdr x))
                         (sum 0 (+ sum (car x))))
                        ((null? x) sum)))
                25)

(unit-test-handler-results)
