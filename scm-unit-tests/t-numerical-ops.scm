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

; primitives = [("+", numAdd),
;             ("-", numSub),
;            ("*", numMul),
;            ("/", numDiv),
;            ("modulo", numericBinop mod),
;            ("quotient", numericBinop quot),
;            ("remainder", numericBinop rem),
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
;
;             ("exact->inexact", numExact2Inexact),
;            ("inexact->exact", numInexact2Exact),

; TODO: test for all types: (=
; TODO: <, >, >=, <=
(unit-test-handler-results)
