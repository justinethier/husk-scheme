
#|
;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases to validate husk's scheme parser
;;
;;
;; #| a nested block comment |#
|#

(load "skim-unit.scm")
(unit-test-start "parser")

(assert/equal 
 (+ 1 2 #| commenting, la, la, la|# 3)
       
              6)
(define a 3)
a
3

(assert/equal (+ 1 2) 3)
(assert/equal ( + 1 2) 3)
(assert/equal (+ 1 2 ) 3)
(assert/equal ( + 1 2 ) 3)
(assert/equal '(a 4) '( a   4 ))
(assert/equal '(2 4) '(2 4))
(assert/equal '(2 . 4) '( 2 . 4))
(assert/equal '(a . 4) '( a . 4 ))
(assert/equal '((a b) . c) '( (a    b) .    c))
(assert/equal ;assert/equal x
                            '(a . 4)
 '(a . 4)                            )

; Cases related to issue #41
(assert/equal '(1 2 . 3) '(1 . (2 . 3)))
(assert/equal '(1 2 . ()) '(1 . (2 . ())))

(unit-test-handler-results)
