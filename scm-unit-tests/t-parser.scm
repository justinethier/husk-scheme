
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

(write (+ 1 2))
(write ( + 1 2))
; TODO: 
(write (+ 1 2 ))
(write ( + 1 2 ))


;(unit-test-handler-results)
