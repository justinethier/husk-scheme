;; JAE - a temporary file used for testing, remove this down the road

 ;; erato.scm -- sieve of Eratosthenes

 ;; execution timings on a 1.9 GHz Intel Core2 + Win7-32

 ;; all primes                < 10^3  < 10^4  < 10^5  < 10^6

 ;; interpreter

 ;; petite (chez scheme) 8.4   0 sec   0 sec   2 sec   2 min
 ;; PLT-r5rs (racket) 5.3.6    0 sec   0 sec   2 sec   4 min
 ;; pi (rhizome/pi) 0.57       0 sec   0 sec   2 sec   6 min
 ;; gsi (gambit) 4.7.0         0 sec   0 sec   4 sec  13 min
 ;; csi (chicken) 4.8.0.5      0 sec   0 sec  10 sec  18 min
 ;; scheme 48 1.9              0 sec   0 sec  10 sec     n/a

 ;; notes
 ;; 1. most of these interpreters come with compilers. i have
 ;; not measured execution timings for compiled code.
 ;; 2. this is only a single application. execution timings 
 ;; vary with application type and complexity of control
 ;; structures.
 ;; 3. rhizome/pi is my favorite interpreter.
(define (append . lst)
  (define append-2
          (lambda (inlist alist) 
                  (foldr (lambda (ap in) (cons ap in)) alist inlist)))
  (if (null? lst)
      lst
      (if (null? (cdr lst))
          (car lst)
          (foldl (lambda (a b) (append-2 b a)) (car lst) (cdr lst)))))

 (define (primes<2n n)
    (erato 2 (* n 2) (list)))

 (define (erato n z primes)

    (define (sieve s)
       (define ok #f)
       (if (null? s) 
        (set! ok #t)
        (if (< n (* (car s) (car s))) (set! ok #t))) ; prime!
       ;(if (or (null? s)                  ; prime
       ;        (< n (* (car s) (car s)))) ; prime!
       (if ok
           (erato (+ n 1) z (append primes (list n)))
           (if (zero? (modulo n (car s))) ; composite
               (erato (+ n 1) z primes)
               (sieve (cdr s)))))

    (if (< n z)
        (sieve primes)
        primes))

((lambda () (primes<2n 1000) 1))
