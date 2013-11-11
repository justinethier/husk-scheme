;; This code is taken from:
;; http://stackoverflow.com/questions/14674165/scheme-generate-random
;;
;; NOTE: Random numbers such as these are good enough for 
;; simple simulations, but beware they are not suitable for
;; cryptographic applications. 
;; 
;; For higher quality random number generators, see:
;; http://programmingpraxis.com/contents/themes/#Random%20Number%20Generators
;;

;; Calling (random) returns a random fraction between 0 (inclusive) and 
;; 1 (exclusive). The random fractions cycle with period m. Calling 
;; (random seed) resets the seed of the random number generator, so that two 
;; random sequences starting from the same seed will be identical; dates in 
;; the form YYYYMMDD make good seeds (that's Knuth's birthday above). If you 
;; want to flip a coin, say: (if (< random 1/2) 'heads 'tails).
;;
;; You probably want to set the seed before using this, for example:
;;
;; (import (scheme time))
;; (random (current-second))
;;
(define random
  (let ((a 69069) (c 1) (m (expt 2 32)) (seed 19380110))
    (lambda new-seed
      (if (pair? new-seed)
          (begin (set! seed (car new-seed)))
          (begin (set! seed (modulo (+ (* seed a) c) m))))
      (/ seed m))))

;; (randint) returns a random integer on the range
;; lo (inclusive) to hi (exclusive); lo defaults to 0:
(define (randint . args)
  (cond ((= (length args) 1) (randint 0 (car args)))
        ((= (length args) 2)
          (inexact->exact
            (+ (car args) (floor (* (random) (- (cadr args) (car args)))))))
        (else (error 'randint "usage: (randint [lo] hi)"))))
