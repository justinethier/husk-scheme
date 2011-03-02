;           (write-string (number->string org) #\space
;                                 (number->string steps) #\linefeed))
           ;(write "n = 1")
           ;(write org)
           ;(write steps))
(define (collatz n org steps)
    (cond ((= n 1) (+ 1 1)) 
          ((even? n) (collatz (/ n 2) org (+ steps 1)))
          (else (collatz (+ 1 (* 3 n)) org (+ steps 1)))))

(define (coller n)
        (collatz n n 0)
        (if (< n 25000)
            (coller (+ n 1))
            (write "done")))

(coller 1)
