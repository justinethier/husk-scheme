;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases specifically for referential transparency.
;;
(load "skim-unit.scm")
(unit-test-start "hygienic macros - referential transparency")

; Testing ability of a nested macro def to use a renamed var
(let ()
 ((lambda (var-02)

  (define-syntax test-template
   (syntax-rules ()
     ((_)
      var-02)))
  
  ((lambda ()
    (assert/equal (test-template) 1)
    (assert/equal (let ((var-02 1)) (test-template)) 1)
    (assert/equal (let ((var-02 2)) (test-template)) 1)
    (define var-02 3)
    (assert/equal (test-template) 1)))) 
  1))

(let ()
  (define var-02 1)
  (define-syntax test-template
   (syntax-rules ()
     ((_)
      var-02)))
  
  ((lambda ()
    (assert/equal (test-template) 1)
    (assert/equal (let ((var-02 1)) (test-template)) 1)
    (assert/equal (let ((var-02 2)) (test-template)) 1)
    (define var-02 3)
    (assert/equal (test-template) 3)))) 

; Variable defined within macro (1) should take precedence over
; the one in the outer env (33)
(define var-02 33)
(let ()
  (define var-02 1)
  (define-syntax test-template
   (syntax-rules ()
     ((_)
      var-02)))
  
  ((lambda ()
    (assert/equal (test-template) 1)
    (assert/equal (let ((var-02 1)) (test-template)) 1)
    (assert/equal (let ((var-02 2)) (test-template)) 1)
    (define var-02 3)
    (assert/equal (test-template) 3)))) 

(define var-02 33)
(let ()
  (set! var-02 1)
  (define-syntax test-template
   (syntax-rules ()
     ((_)
      var-02)))
  
  ((lambda ()
    (assert/equal (test-template) 1)
    (assert/equal (let ((var-02 1)) (test-template)) 1)
    (assert/equal (let ((var-02 2)) (test-template)) 1)
    (define var-02 3)
    (assert/equal (test-template) 3)))) 
(unit-test-handler-results)
