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

; From http://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_12.html
; This test case (when it passes) proves that the macro subsystem
; rewrites variables to prevent collisions with top-level identifiers.
(assert/equal (let ((=> #f)) (cond (#t => 'ok)))
              'ok)

(unit-test-handler-results)
