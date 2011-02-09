;
; husk-scheme
; http://github.com/justinethier/husk-scheme
;
; Written by Justin Ethier
;
; Test cases for do form
;
(load "skim-unit.scm")

(assert/equal 
                (do ((vec (make-vector 5) vec)
                     (i 0 (+ i 1)))
                    ((= i 5) vec)
                     (vector-set! vec i i))
                #(0 1 2 3 4))

; Per spec, <step> does not have to be specified
(assert/equal 
                (do ((vec (make-vector 5))
                     (i 0 (+ i 1)))
                    ((= i 5) vec)
                     (vector-set! vec i i))
                #(0 1 2 3 4))

(assert/equal 
                (let ((x '(1 3 5 7 9)))
                    (do ((x x (cdr x))
                         (sum 0 (+ sum (car x))))
                        ((null? x) sum)))
                25)

(unit-test-handler-results)
