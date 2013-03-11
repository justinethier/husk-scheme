;;;
;;; husk-scheme
;;; http://github.com/justinethier/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; Test cases from:
;;; http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_chap_6
(unit-test-start "equality procedures")

(assert/equal
    (eqv? 'a 'a)
    #t)
(assert/equal
    (eqv? 'a 'b)
    #f)
(assert/equal
    (eqv? 2 2)
    #t)
(assert/equal
    (eqv? '() '())
    #t)
(assert/equal
    (eqv? 100000000 100000000)
    #t)
(assert/equal
    (eqv? (cons 1 2) (cons 1 2))
     #f)
(assert/equal
    (eqv? (lambda () 1)
          (lambda () 2))
    #f)
(assert/equal
    (eqv? #f 'nil)
    #f)

; R5RS test case
(assert/equal
    (let ((p (lambda (x) x)))
         (eqv? p p))
    #t)

(define gen-counter
    (lambda ()
          (let ((n 0))
                  (lambda () (set! n (+ n 1)) n))))

;Test case from spec:
(assert/equal
    (let ((g (gen-counter)))
        (eqv? g g))
    #t)
; TODO: should be #f per spec, but is not
(assert/equal (eqv? (gen-counter) (gen-counter)) 
              #f)

(define gen-loser
    (lambda ()
          (let ((n 0))
                  (lambda () (set! n (+ n 1)) 27))))
;Test case from spec:
(assert/equal
    (let ((g (gen-loser)))
        (eqv? g g))
    #t)

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
    #f)

(assert/equal
    (let ((x '(a)))
        (eqv? x x))
    #t)

(assert/equal (eq? 'a 'a) #t)
; TODO: each of these return values disagrees with the spec
;(assert/equal (eq? '(a) '(a)) #t)
;(assert/equal (eq? (list 'a) (list 'a))  #f)
;(assert/equal (eq? "a" "a") #t)
;(assert/equal (eq? "" "") #t)
(assert/equal (eq? '() '())              #t)
(assert/equal (eq? 2 2) #t)
(assert/equal (eq? #\A #\A) #t)
(assert/equal (eq? car car)              #t)
(assert/equal (let ((n (+ 2 3)))
                   (eq? n n))
              #t) ;  ===>  unspecified
(assert/equal (let ((x '(a)))
    (eq? x x))              #t)
(assert/equal (let ((x '#()))
    (eq? x x))              #t)
(assert/equal (let ((p (lambda (x) x)))
    (eq? p p))              #t)

(assert/equal (equal? 'a 'a) #t)
(assert/equal (equal? '(a) '(a))         #t)
(assert/equal (equal? '(a (b) c)
                '(a (b) c))              #t)
(assert/equal (equal? "abc" "abc")       #t)
(assert/equal (equal? 2 2)               #t)
(assert/equal (equal? (make-vector 5 'a)
                (make-vector 5 'a))      #t)
(assert/equal (equal? (lambda (x) x)
                      (lambda (y) y))
               #f) ; ===>  unspecified

(assert/equal (not #t)         #f)
(assert/equal (not 3)          #f)
(assert/equal (not (list 3))   #f)
(assert/equal (not #f)         #t)
(assert/equal (not '())        #f)
(assert/equal (not (list))     #f)
(assert/equal (not 'nil)       #f)

(assert/equal (boolean? #f)     #t)
(assert/equal (boolean? 0)      #f)
(assert/equal (boolean? '())    #f)

(unit-test-handler-results)
