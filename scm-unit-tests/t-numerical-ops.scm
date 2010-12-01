(load "skim-unit.scm")

(assert/equal (complex? 3+4i) #t)
(assert/equal (complex? 3) #t)
(assert/equal (real? 3) #t)
(assert/equal (real? -2.5+0.0i) #t)
;(assert/equal (real? #e1e103) #t)
(assert/equal (rational? 6/10) #t)
(assert/equal (rational? 6/3) #t)
;(assert/equal (integer? 3+0i) #t)
;(assert/equal (integer? 3.0) #t)
;(assert/equal (integer? 8/4) #t)


(assert/equal (max 3 4) 4)
(assert/equal (max 3.9 4) 4) ; TODO: technically not to spec

(assert/equal (+ 1 2 3 4 5) 15)
(assert/equal (+ 4/8 5) 44/8)
(assert/equal (+ 1/1 2 3.0 4 10/2) 15.0)
(assert/equal (+ 1+0i 2+0i) 3+0i)
(assert/equal (- 1 2) -1)
(assert/equal (* 2 3) 6)
(assert/equal (* 4/2 6/2) 6/1)
(assert/equal (* 8/4 6/2) 24/4)
(assert/equal (* 3 1+1i) 3+3i)
(assert/equal (/ 2 1) 2)
(assert/equal (/ 1 2) 0)
(assert/equal (/ 1.0 2) 0.5)
(assert/equal (/ 1/1 2/1) 1/2)
(assert/equal (modulo 8 2) 0)
(assert/equal (modulo 9 2) 1)
(assert/equal (quotient 44 2) 22)

(assert/equal (modulo 13 -4) -3)
(assert/equal (remainder 13 -4) 1)
(assert/equal (modulo -13 -4) -1)
(assert/equal (remainder -13 -4) -1)
;TODO: - support for inexact - (remainder -13 -4.0)            ===>  -1.0  ; inexact

;(assert/equal () )

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

;(gcd 32 -36)                    ===>  4
;(gcd)                           ===>  0
;(lcm 32 -36)                    ===>  288
;(lcm 32.0 -36)                  ===>  288.0  ; inexact
;(lcm)                           ===>  1

(assert/equal (exact->inexact 2) 2.0)
(assert/equal (inexact->exact 2.0) 2)

(assert/equal (= 1 (+ 0 1)) #t)
(assert/equal (= 1.0 1) #t)
(assert/equal (= 1 4/4) #t)
(assert/equal (= 10 10+0i) #t)
(assert/equal (> 2 1) #t)
(assert/equal (> 2 1.9) #t)
(assert/equal (> 2 1/9) #t)
(assert/equal (>= 2 1) #t)
(assert/equal (>= 2 1.9) #t)
(assert/equal (>= 2 1/9) #t)
(assert/equal (>= 2 2) #t)
(assert/equal (>= 2 2.0) #t)
(assert/equal (>= 2 18/9) #t)
(assert/equal (<= 2 2.0) #t)
(assert/equal (<= 2 18/9) #t)
(assert/equal (< 2 2.0) #f)
(assert/equal (< 2 18/9) #f)
(unit-test-handler-results)
