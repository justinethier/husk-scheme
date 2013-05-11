;;;
;;; husk-scheme
;;; http://github.com/justinethier/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; Test cases from:
;;; http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_chap_6
;;;
;;;
;;; NOTE: many test cases are commented-out as they fail due to differences between
;;;       the (Haskell) memory model we are using and the memory model the spec
;;;       assumes. This may be addressed at some point, but is not currently a 
;;;       priority item.
;;;
;;;       For more information: https://github.com/justinethier/husk-scheme/wiki/Compatibility
;;;
(unit-test-start "standard procedures")

(assert/equal
    (eqv? 'a 'a)
    #t
    "a symbols eqv")
(assert/equal
    (eqv? 'a 'b)
    #f
    "a/b symbols not eqv")
(assert/equal
    (eqv? 2 2)
    #t
    "numbers eqv")
(assert/equal
    (eqv? '() '())
    #t
    "nil lists eqv")
(assert/equal
    (eqv? 100000000 100000000)
    #t
    "large numbers eqv")
; Fails R5RS test case
;(assert/equal
;    (eqv? (cons 1 2) (cons 1 2))
;     #f)
(assert/equal
    (eqv? (lambda () 1)
          (lambda () 2))
    #f
    "functions not eqv")
(assert/equal
    (eqv? #f 'nil)
    #f
    "bool / nil not eqv")

; R5RS test case
(assert/equal
    (let ((p (lambda (x) x)))
         (eqv? p p))
    #t
    "p variables eqv")

(define gen-counter
    (lambda ()
          (let ((n 0))
                  (lambda () (set! n (+ n 1)) n))))

;Test case from spec:
(assert/equal
    (let ((g (gen-counter)))
        (eqv? g g))
    #t
    "g gen counter eqv")

(define gen-loser
    (lambda ()
          (let ((n 0))
                  (lambda () (set! n (+ n 1)) 27))))
;Test case from spec:
(assert/equal
    (let ((g (gen-loser)))
        (eqv? g g))
    #t
    "g gen loser eqv")

;(eqv? (gen-loser) (gen-loser))
;                                ===>  unspecified

;(letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
;                  (g (lambda () (if (eqv? f g) 'both 'g))))
;    (eqv? f g))
;                                ===>  unspecified

(assert/equal
    (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
                  (g (lambda () (if (eqv? f g) 'g 'both))))
        (eqv? f g))
    #f
    "f / g not eqv")

(assert/equal
    (let ((x '(a)))
        (eqv? x x))
    #t
    "x eqv")

; Many tests that do not meet spec
(assert/equal (eq? 'a 'a) #t "a's eq")
(assert/equal (eq? '(a) '(a)) #t "a lists eq")
;(assert/equal (eq? (list 'a) (list 'a))  #f)
(assert/equal (eq? "a" "a") #t "a strings eq")
(assert/equal (eq? "" "") #t "empty strings eq")
(assert/equal (eq? '() '())              #t)
(assert/equal (eq? 2 2) #t "2's eq")
(assert/equal (eq? #\A #\A) #t "A chars eq")
;(assert/equal (eq? car car)              #t)
(assert/equal (let ((n (+ 2 3)))
                   (eq? n n))
              #t
              "n sum eq") ;  ===>  unspecified
(assert/equal (let ((x '(a)))
    (eq? x x))              #t "x as a list eq")
(assert/equal (let ((x '#()))
    (eq? x x))              #t "x as vector eq")
;(assert/equal (let ((p (lambda (x) x)))
;    (eq? p p))              #t)

(assert/equal (equal? 'a 'a) #t "a's equal")
(assert/equal (equal? '(a) '(a))         #t "a lists equal")
(assert/equal (equal? '(a (b) c)
                '(a (b) c))              #t "bigger list equal")
(assert/equal (equal? "abc" "abc")       #t "abc string equal")
(assert/equal (equal? 2 2)               #t "2 equal")
(assert/equal (equal? (make-vector 5 'a)
                (make-vector 5 'a))      #t "vectors equal")
(assert/equal (equal? (lambda (x) x)
                      (lambda (y) y))
               #f "functions not equal") ; ===>  unspecified

(assert/equal (not #t)         #f "not true")
(assert/equal (not 3)          #f "not 3")
(assert/equal (not (list 3))   #f "not list 3")
(assert/equal (not #f)         #t "not false")
(assert/equal (not '())        #f "not nil")
(assert/equal (not (list))     #f "not list")
(assert/equal (not 'nil)       #f "not nil symbol")

(assert/equal (boolean? #f)     #t "false is a boolean")
(assert/equal (boolean? 0)      #f "0 is not a boolean")
(assert/equal (boolean? '())    #f "nil is not a boolean")

(unit-test-handler-results)
