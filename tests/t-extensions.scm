;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for non-standard extentions 
;;
(unit-test-start "non-standard extensions")

; It is difficult to test this directly since adding additional
; code can affect the number appended to the symbol.
;
; Anyway, gensym has to work in order for macro hygiene, so
; this is validated indirectly by those test cases.
;
;(assert/equal 
;  (gensym "test")
;  'test4)

(assert/equal 
  (expand (let () 1)) 
 '((lambda () 1)))

(unit-test-handler-results)
