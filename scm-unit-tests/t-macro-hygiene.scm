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

(let-syntax ((test-template-2
 (syntax-rules (v)
   ((_)
    v))))
    
    (assert/equal (test-template-2) 3)
    (assert/equal (let ((v 1)) (test-template-2)) 3)
    (assert/equal (let ((v 2)) (test-template-2)) 3)
    (define v 3)
    (assert/equal (test-template-2) 3))

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

; TODO: Issue #53:
#|
; Test cases fail below because var-02 is not actually defined
; in the env until after the macros are already expanded...
; So, since the var is not defined in defEnv, the "other side" of
; hygiene is not respected in this case.
;
;(lambda ()
(let ((var-02 1))
;(let ()
;  (define var-02 1)
  (define-syntax test-template
   (syntax-rules ()
     ((_)
      var-02)))
  (assert/equal (test-template) 1)
  (assert/equal (let ((var-02 1)) (test-template)) 1)
  (assert/equal (let ((var-02 2)) (test-template)) 1)
  (define var-02 3)
  (assert/equal (test-template) 3)
)
; TODO: above, need to return the internal vars pass-count and fail-count since within a function
; the correct var is not set. Or is this a bug in set! because it creates a new var instead
; of modifying the one in the parent env?
; UPDATE - this may just be when lambda is used, still need to track it down
|#

; Example without a literal identifier; variable is referenced
; directly from the template

; End other side of hygiene in the template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Test case from
; http://en.wikipedia.org/wiki/Hygienic_macro
(define-syntax swap!
     (syntax-rules ()
         ((_ a b)
          (let ((temp a))
            (set! a b)
            (set! b temp)))))
(define swap-x 1)
(define swap-y 2)
(swap! swap-x swap-y)
(assert/equal swap-x 2)
(assert/equal swap-y 1)


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

; Example #2 from Macros that Work
(assert/equal (let ((x "outer"))
  (let-syntax ((m (syntax-rules () ((m) x))))
    (let ((x "inner"))
    (m))))
 "outer")

; Examples from the source to R5RS pitfall 3.3
(assert/equal
  (let ((a 1)) 
     (letrec-syntax 
         ((foo (syntax-rules () 
                 ((_ b) 
                  (bar a b)))) 
          (bar (syntax-rules () 
                 ((_ c d) 
                  (cons c (let ((c 3)) 
                            (list d c 'c))))))) 
       (let ((a 2)) 
         (foo a)))) 
  '(1 2 3 a))

(assert/equal 
  (let ((a 1)) 
     (define-syntax 
         foo (syntax-rules () 
                 ((_ b) 
                  (bar a b)))) 
     (define-syntax
          bar (syntax-rules () 
                 ((_ c d) 
                  (cons c (let ((c 3)) 
                            (list d c 'c)))))) 
       (let ((a 2)) 
         (foo a)))
  '(1 2 3 a))

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

; Examples from/based on pitfall 8.3
(assert/equal 1
  (let ((x 1))
    (let-syntax ((foo (syntax-rules () ((_) 2))))
      (define x (foo))
      3)
    x))
(assert/equal 1
  (let ((x 1))
    (letrec-syntax ((foo (syntax-rules () ((_) 2))))
      (define x (foo))
      3)
    x))

(unit-test-handler-results)
