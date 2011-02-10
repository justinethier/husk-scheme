;
; Greatest common denominator
; Based on example code from SCIP
;
(load "stdlib.scm")

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (main args)
  (write (gcd
    (string->number (list-ref args 1))
    (string->number (list-ref args 2)))))
