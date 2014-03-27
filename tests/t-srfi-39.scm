;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for SRFI-39 (Parameter Objects)
;;
(unit-test-start "SRFI 39: Parameter Objects")
(define radix
  (make-parameter
   10
   (lambda (x)
     (if (and (exact-integer? x) (<= 2 x 16))
         x
         (error "invalid radix")))))

(define (f n) (number->string n (radix)))

(assert/equal (f 12) "12")
(assert/equal
  (parameterize ((radix 2)) 
    (f 12)) 
  "1100")
(unit-test-handler-results)
