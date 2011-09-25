;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases to ensure hygiene is preserved by macros
;;
(load "skim-unit.scm")
(unit-test-start "macro hygiene")

(define-syntax orr 
  (syntax-rules () 
   ((orelse <expr1> <expr2>) 
    (let ((temp <expr1>)) 
         (if temp temp <expr2>)))))

; TODO: this is broken currently
; see Issue #30
(assert/equal
    (let ((temp 4)) (orr #f temp))
    4)
;=> #f instead of 4

; TODO: test cases for each of the 4 classes of problems identified in Macros that Work:
;
; We can now identify four classes of capturing problems.
; 
; The first class involves inadvertent capturing by bound variables introduced by a macro. The second class involves the inadvertent capture of free variable references introduced by a macro. The third is like the first, but involves inadvertent capturing by bindings of syntactic keywords (i.e. local macros) introduced by a macro. The fourth is like the second, but involves the inadvertent capture of references to syntactic keywords introduced by a macro.
;


 
(unit-test-handler-results)
