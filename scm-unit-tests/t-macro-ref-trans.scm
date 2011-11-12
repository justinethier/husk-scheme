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

(define var-02 34)
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

(let ()
 ((lambda (var-12)

  (let-syntax ((test-template-ls ;; TODO: should also pass if this is defined as 'test-template'
   (syntax-rules ()
     ((_)
      var-12))))
  
  ((lambda ()
    (assert/equal (test-template-ls) 1)
    (assert/equal (let ((var-12 1)) (test-template-ls)) 1)
    (assert/equal (let ((var-12 2)) (test-template-ls)) 1)
    (define var-12 3)
    (assert/equal (test-template-ls) 1)))) 
  ) 1))

(let-syntax ()
 ((lambda (var-12)

  (let-syntax ((test-template-ls
   (syntax-rules ()
     ((_)
      var-12))))
  
  ((lambda ()
    (assert/equal (test-template-ls) 1)
    (assert/equal (let ((var-12 1)) (test-template-ls)) 1)
    (assert/equal (let ((var-12 2)) (test-template-ls)) 1)
    (define var-12 3)
    (assert/equal (test-template-ls) 1)))) 
  ) 1))

; A series of test cases demonstrating that divert works
; in the case of a nested let-syntax expression.
;
; In this example, x is diverted back into the enclosing env.
((lambda ()
    (define x 1)
    (define-syntax test
     (syntax-rules (x)
      ((_)
       x)))
    
    ;(let () ;-syntax ()
    (let-syntax ()
     ((lambda (var-02)
    
      (let-syntax ((test-template
       (syntax-rules ()
         ((_)
          var-02))))
      
      ((lambda ()
        (assert/equal (test-template))
        (assert/equal (let ((var-02 1)) (test-template)))
        (assert/equal (let ((var-02 2)) (test-template)))
        (assert/equal (let ((var-02 2)) (test)))
        (define var-02 3)
        (assert/equal (test-template))))) 
      ) 1))))

(unit-test-handler-results)
