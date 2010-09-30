(load "skim-unit.scm")

(assert-equal (lambda () (complex? 3+4i)) #t)
(assert-equal (lambda () (complex? 3)) #t)
(assert-equal (lambda () (real? 3)) #t)
;(assert-equal (lambda () (real? -2.5+0.0i)) #t)
;(assert-equal (lambda () (real? #e1e103)) #t)
(assert-equal (lambda () (rational? 6/10)) #t)
(assert-equal (lambda () (rational? 6/3)) #t)
;(assert-equal (lambda () (integer? 3+0i)) #t)
;(assert-equal (lambda () (integer? 3.0)) #t)
;(assert-equal (lambda () (integer? 8/4)) #t)


(assert-equal (lambda () (max 3 4)) 4)
(assert-equal (lambda () (max 3.9 4)) 4) ; TODO: technically not to spec

(assert-equal (lambda () (+ 1 2 3 4 5)) 15)
(assert-equal (lambda () (+ 4/8 5)) 44/8)
(assert-equal (lambda () (+ 1/1 2 3.0 4 10/2)) 15.0)
(assert-equal (lambda () (+ 1+0i 2+0i)) 3+0i)
(assert-equal (lambda () (- 1 2)) -1)
(assert-equal (lambda () (* 2 3)) 6)
(assert-equal (lambda () (* 4/2 6/2)) 6/1)
(assert-equal (lambda () (* 8/4 6/2)) 24/4)
(assert-equal (lambda () (* 3 1+1i)) 3+3i)
(assert-equal (lambda () (/ 2 1)) 2)
(assert-equal (lambda () (/ 1 2)) 0)
(assert-equal (lambda () (/ 1.0 2)) 0.5)
(assert-equal (lambda () (/ 1/1 2/1)) 1/2)
(assert-equal (lambda () (modulo 8 2)) 0)
(assert-equal (lambda () (modulo 9 2)) 1)
(assert-equal (lambda () (quotient 44 2)) 22)

(assert-equal (lambda () (modulo 13 -4)) -3)
(assert-equal (lambda () (remainder 13 -4)) 1)
(assert-equal (lambda () (modulo -13 -4)) -1)
(assert-equal (lambda () (remainder -13 -4)) -1)
;TODO: - support for inexact - (remainder -13 -4.0)            ===>  -1.0  ; inexact

;(assert-equal (lambda () ()) )

;
;            ("round", numRound),
;            ("floor", numFloor),
;            ("ceiling", numCeiling),
;            ("truncate", numTruncate),
;
;            ("numerator", numNumerator),
;           ("denominator", numDenominator),
;
;            ("exp", numExp), 
;            ("log", numLog), 
;            ("sin", numSin), 
;            ("cos", numCos), 
;            ("tan", numTan), 
;            ("asin", numAsin),
;            ("acos", numAcos), 
;            ("atan", numAtan),
;
;            ("sqrt", numSqrt),
;            ("expt", numExpt),
;
;            ("make-rectangular", numMakeRectangular),
;            ("make-polar", numMakePolar), 
;            ("real-part", numRealPart ), 
;            ("imag-part", numImagPart), 
;             ("magnitude", numMagnitude), 
;             ("angle", numAngle ), 

(assert-equal (lambda () (exact->inexact 2)) 2.0)
(assert-equal (lambda () (inexact->exact 2.0)) 2)

(assert-equal (lambda () (= 1 (+ 0 1))) #t)
(assert-equal (lambda () (= 1.0 1)) #t)
(assert-equal (lambda () (= 1 4/4)) #t)
(assert-equal (lambda () (= 10 10+0i)) #t)
(assert-equal (lambda () (> 2 1)) #t)
(assert-equal (lambda () (> 2 1.9)) #t)
(assert-equal (lambda () (> 2 1/9)) #t)
(assert-equal (lambda () (>= 2 1)) #t)
(assert-equal (lambda () (>= 2 1.9)) #t)
(assert-equal (lambda () (>= 2 1/9)) #t)
(assert-equal (lambda () (>= 2 2)) #t)
(assert-equal (lambda () (>= 2 2.0)) #t)
(assert-equal (lambda () (>= 2 18/9)) #t)
(assert-equal (lambda () (<= 2 2.0)) #t)
(assert-equal (lambda () (<= 2 18/9)) #t)
(assert-equal (lambda () (< 2 2.0)) #f)
(assert-equal (lambda () (< 2 18/9)) #f)
(unit-test-handler-results)
