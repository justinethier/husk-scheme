;
; Greatest common denominator
; Based on example code from SCIP
;
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; TODO: load argv into (main arguments)
(gcd 2010 42)
