
;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases to validate husk's scheme parser
;;

;(load "skim-unit.scm")
;(unit-test-start "parser")

;(assert/equal 
(write (+ 1 2 #| commenting, la, la, la|# 3)
       
       )
;              6)
(define a 3)
a
3

(write (+ 1 2))
(write ( + 1 2))
(write (+ 1 2 ))
(write ( + 1 2 ))
(write '(a 4))
(write '(2 4))
(write '(2 . 4))
(write '(a . 4))
(write '((a b) . c))
(write ;assert/equal x
                            '(a . 4)
                            )

;(unit-test-handler-results)
