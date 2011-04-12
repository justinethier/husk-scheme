;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for cond form 
;;
(load "skim-unit.scm")
(unit-test-start "cond")

(assert/equal (cond ((> 3 2) 'greater) ((< 3 2) 'less))
			  'greater)
(assert/equal (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))
			  'equal)

; special form, =>
(assert/equal (cond ((assv 'b '((a 1) (b 2))) => cadr)
                    (else #f))
              2)

(unit-test-handler-results)
