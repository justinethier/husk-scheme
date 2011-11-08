;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; More test cases for macros, specifically to test macros using
;; improper and proper lists.
;;
(load "skim-unit.scm")
(unit-test-start "hygienic macros")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test cases to demonstrate how to deal with
; 'other side of hygiene' when literal is in the template

; Case where the template is just an atom
(define v 1)
(define-syntax test-template
 (syntax-rules (v)
   ((_)
    v)))

(assert/equal (test-template) 1)
(assert/equal (let ((v 1)) (test-template)) 1)
(assert/equal (let ((v 2)) (test-template)) 1)
(define v 3)
(assert/equal (test-template) 3)

; Simple example of a list-based template
(define v1 1)
(define-syntax test-template
 (syntax-rules (v1)
   ((_)
    (+ v1))))

(assert/equal (test-template) 1)
(assert/equal (let ((v1 1)) (test-template)) 1)
((lambda (v1) (let ((v1 2)) (test-template))) 10)
(define v1 3)
(assert/equal (test-template) 3)
#|
(let ()
  (define var-02 1)
  (define-syntax test-template
   (syntax-rules ()
     ((_)
      var-02)))
  
  (assert/equal (test-template) 1)
  (assert/equal (let ((var-02 1)) (test-template)) 1)
  (assert/equal (let ((var-02 2)) (test-template)) 1)
  (define var-02 3)
  (assert/equal (test-template) 3))
|#
; Example without a literal identifier; variable is referenced
; directly from the template

; End other side of hygiene in the template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define var 'original)

(define-syntax test-var
 (syntax-rules (var)
   ((_ var)
    var)
   ((_ pattern-var)
    'no-match)))

; Should return var since var is same in env/use 
(assert/equal (test-var var)
              'original)

; Should be unmatched because of the difference between env/use
(assert/equal (let ((var 1)) (test-var var))
              'no-match)

; Again, should be unmatched because of the difference between env/use
; BUT, it is matched due to how scheme implementations deal with the global env
(define var 'new-var)
(assert/equal (test-var var)
              'new-var)





#|
; if marked as a literal identifier so it is not loaded as a pattern variable...
(define-syntax test
 (syntax-rules (if)
   ((_ if)
    (if #t #t #f))
   ((_ a)
    'no-match)))

; if is introduced by (let) so it should not match in the macro because
; it is defined differently in env(use) and env(def):
(write
  (let ((if (lambda (x y z) "func is called"))) (test if)))

; But here there is no definition in env(use) so it does match the first rule
(write
  (test if))


; In this case, if should be taken from the def environment (IE, the special form) and
; not from the use environment (which is the "should not happen" function.
;
; BUT, I'm confused, why does the macro match at all?
(define if (lambda (x y z) "should not happen"))
;(define if 1)
(write
  (test if))

; Since the new if is now in the def environment, it should execute the function this time
(define-syntax test-2
 (syntax-rules (if)
   ((_ if)
    (if #t #t #f))))
(write
  (test-2 if))
|#

(unit-test-handler-results)
