;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for eval 
;;
(unit-test-start "eval")

(assert/equal (eval '(* 7 3))
              21)

(assert/equal (let ((f (eval '(lambda (f x) (f x x)))))
                    (f + 10))
              20)

(unit-test-handler-results)
