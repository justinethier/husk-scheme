;;;
;;; husk-scheme
;;; http://github.com/justinethier/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; Test cases to ensure hygiene is preserved by macros
;;;
;(load "skim-unit.scm")
;(unit-test-start "macro hygiene")
;
(define-syntax orr 
  (syntax-rules () 
   ((orelse <expr1> <expr2>) 
    (let ((temp <expr1>)) 
         (if temp temp <expr2>)))))

; TODO: this is broken currently
; see Issue #30
;(assert/equal
(write
    (let ((temp 4)) (orr #f temp))
)
;    4)
;=> #f instead of 4
;
;;; TODO: test cases for each of the 4 classes of problems identified in Macros that Work:
;;;
;;; We can now identify four classes of capturing problems.
;;; 
;;; The first class involves inadvertent capturing by bound variables introduced by a macro. The second class involves the inadvertent capture of free variable references introduced by a macro. The third is like the first, but involves inadvertent capturing by bindings of syntactic keywords (i.e. local macros) introduced by a macro. The fourth is like the second, but involves the inadvertent capture of references to syntactic keywords introduced by a macro.
;;;
;
;; example of 1st class
;(define-syntax discriminant
;  (syntax-rules ()
;  ((discriminant ?a ?b ?c)
;   (let ((temp ?b))
;    (- (* temp temp) (* 4 ?a ?c))))))
;
;(assert/equal 
;    (let ((temp 1))
;        (discriminant temp 2 3))
;    -8) ; Above should be -8
;
;; TODO: example of 2nd class
;; Unfortunately the paper's example uses (let-syntax) which of course we do not support...
;;(let-syntax ((first
;;
;;(syntax-rules
;;
;;((first ?x) => (car ?x))))
;;
;;(second
;;
;;(syntax-rules
;;
;;((second ?x) => (car (cdr ?x))))))
;;
;;(let ((car "duesenberg"))
;;
;;(let-syntax ((classic
;;
;;(syntax-rules
;;
;;((classic) => car))))
;;
;;(let ((car "yugo"))
;;
;;(let-syntax ((affordable
;;
;;(syntax-rules
;;
;;((affordable) => car))))
;;
;;(let ((cars (list (classic)
;;
;;(affordable))))
;;
;;(list (second cars)
;;
;;(first cars))))))))
;;
;#|
;; An example for the third class of capturing problems
;(define-syntax push
;  (syntax-rules ()
;    ((push ?v ?x) 
;      (set! ?x (cons ?v ?x)))))
;
;(display
;(let ((pros (list "cheap" "fast"))
;  (cons (list)))
;  (push "unreliable" cons)
;  cons)
;) ; Expected: ("unreliable")
;|#
;
;; Example of fourth class 
;; TODO: not exactly sure how to work code from paper into a test case
;;(define-syntax let-ex
;;  (syntax-rules ()
;;    ((let-ex ((?name ?val)) ?body)
;;     ((lambda (?name) ?body) ?val))))
;
;
;(write "TODO: example 1 from the paper")
;(write "TODO: example 2 from the paper")
;(write "TODO: example 3 from the paper")
;(unit-test-handler-results)

; FUTURE: once this works, should also test the let-syntax version
;(define-syntax push
;  (syntax-rules ()
;    ((push ?v ?x)
;     (set! ?x (cons ?v ?x)))))
;
;(let ((pros (list "cheap" "fast"))
;      (cons (list)))
;  (push "unreliable" cons)
;  cons)

; Questions about the paper:
; why is e1 |- list -> list?
; why is list not a free identifier but the other symbols are?
; is it because of the algorithm's equations in Fig 2?

(define-syntax test
  (syntax-rules ()
    ((_)
     ((lambda (very-long-name) 'very-long-name) 1))))
(test)
