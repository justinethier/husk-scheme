;;
;; A simple unit testing function.
;;
;; This can be simplified using (begin) and (newline), 
;; but neither is supported by huskc at this time.
;;
(define (assert-equal test-id value expression)
  (if (eqv? value expression)
    ((lambda () 
       (display (string-append "Passed: " (number->string test-id)))
       (display #\newline)))

    ((lambda () 
       (display (string-append "Failed: " (number->string test-id)))
       (display ", expected [")
       (display value)
       (display "], got [")
       (display expression)
       (display "].")
       (display #\newline)))))

(assert-equal 1.1 1 1)
(assert-equal 1.2 1 2)

1
(if 1 2 3)

(define b 1)
(assert-equal 1.3 1 b)

(define (a x) x)
(assert-equal 1.4 1234567890 (a 1234567890))

(lambda () 1)

(assert-equal 1.5 10  (((lambda () +)) 1 2 3 4))
(assert-equal 1.6 12  ((lambda (a b c) (+ a b c)) 2 4 6))
(assert-equal 1.7 #f  (if #f #t #f))
(assert-equal 1.8 100 (if ((lambda () #t)) (+ 100) (+ 200)))
(assert-equal 1.9 200 (if ((lambda () #f)) (+ 100) (+ 200)))
1
2
(assert-equal 1.10 6 (+ 1 2 3))
(assert-equal 1.11 14 (+ (+ 1) (+ (+ 2 2)) (+ (+ 3 3) (+ (+ 3)))))
4
1
2
(assert-equal 1.12 66 (+ 11 22 33))
3
1
2
(assert-equal 1.13 3 3)
2
(assert-equal 1.14 15 (+ 1 2 (+ 3 4 5)))
(assert-equal 1.15 21 (+ 1 2 (+ 3 (+ 4 6) 5)))
(assert-equal 1.16 16 (+ 1 (+ 2) (+) 3 (- 9 2) (/ 9 3)))
(assert-equal 1.17 16 (+ 1 (+ 2) 3 (- 9 2) (/ 9 3)))
(assert-equal 1.18 15 (+ (+ 2) 3 (- 9 2) (/ 9 3)))
(assert-equal 1.19 13 (+ 3 (- 9 2) (/ 9 3)))
(assert-equal 1.20 10 (+ (- 9 2) (/ 9 3)))
(assert-equal 1.21 3 (+ (/ 9 3))) ; results are OK
(assert-equal 1.22 16 (if (+ 1 (+ 2) (+) 3 (- 9 2) (/ 9 3)) 
           (+ 1 (+ 2) (+) 3 (- 9 2) (/ 9 3))
           (+ 1 (+ 2) (+) 3 (- 9 2) (/ 9 3))))
(assert-equal 1.23 3 (if 1 (+ 2 1) (+ 3 1)))
(assert-equal 1.24 7 (if 1 (+ 2 1 4) 3))
(assert-equal 1.25 2 (if 1 2 3))
(assert-equal 1.26 7 (if 1 (+ 2 1 4) 3))
(assert-equal 1.27 2 (if (+ 1) 2 3))
(assert-equal 1.28 #f (if #f 2 #f))
(assert-equal 1.29 22 (if (+ 1) 22 3))

(assert-equal 1.30 3 ((lambda () 1 2 (+ 3) )))
(assert-equal 1.31 3 ((lambda () 1 2 3 )))
((lambda () 1 2 (+ 3 4)))
(assert-equal 1.32 3 (if #t (+ ((lambda () 1 2 3) )) 4))

(define (f . a) a)
(assert-equal 2.1 '(1 2 3) (f 1 2 3))


