;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Unit tests for the standard library
;;
(unit-test-start "standard library")

; Primitive expression types, tests from R5RS spec
; (consider moving to a separate file)
(define x 28)
(assert/equal x 28)

(assert/equal 'a  'a)
(assert/equal '#(a b c) '#(a b c))
(assert/equal '() '())
(assert/equal '(+ 1 2) '(+ 1 2))
(assert/equal '(quote a) '(quote a))
(assert/equal ''a       '(quote a))

(assert/equal '"abc"  "abc")
(assert/equal '145932 145932)
(assert/equal '#t  #t)

(assert/equal (+ 3 4)  
              7)
(assert/equal ((if #f + *) 3 4)  
              12)

(assert/equal ((lambda (x) (+ x x)) 4)  8)

(define reverse-subtract
    (lambda (x y) (- y x)))
(assert/equal (reverse-subtract 7 10)  3)

(define add4
    (let ((x 4))
          (lambda (y) (+ x y))))
(assert/equal (add4 6)  10)

(assert/equal (if (> 3 2) 'yes 'no)   'yes)
(assert/equal (if (> 2 3) 'yes 'no)   'no)
(assert/equal (if (> 3 2)
                  (- 3 2)
                  (+ 3 2))
              1)

(define x 2)
(assert/equal (+ x 1)  3)
(set! x 4)
(assert/equal (+ x 1) 5)

; Pairs and Lists section, from R5RS spec
(define x (list 'a 'b 'c))
(define y x)
(assert/equal y  
              '(a b c))
(assert/equal (list? y)  
              #t)
(set-cdr! x 4)
(assert/equal x 
              '(a . 4))

(define f '(1 2 3 4))
(set-car! f 8)
(assert/equal f '(8 2 3 4))

(define f '(1 . 2))
(set-car! f "a")
(assert/equal f '("a" . 2))

; TODO: should be able to get these to pass now...
;
; Looks like all of below fail because (define y x) 
; is assigning to to the evaluated value of x instead of the symbol x.
;
; Not sure if we can resolve this or if it is really just a fundamental problem
; since we wrote the interpreter in Haskell. For now the code stands as-is and
; this is listed as a compatibility issue in the husk wiki.
;
;;(assert/equal (eqv? x y) #t)
;;(assert/equal y '(a . 4))
;;(assert/equal (list? y) #f)
;;
; End test failures

(set-cdr! x x)
(assert/equal (list? x)  #f)

(assert/equal (pair? (list 1 2 3)) #t)
(assert/equal (pair? 1) #f)
(assert/equal (pair? '(a . b)) #t)
(assert/equal (pair? '(a b c)) #t)
(assert/equal (pair? '()) #f)
(assert/equal (pair? '#(a b)) #f)


(assert/equal (cons 'a '())            '(a))
(assert/equal (cons '(a) '(b c d))     '((a) b c d))
(assert/equal (cons "a" '(b c))        '("a" b c))
(assert/equal (cons 'a 3)              '(a . 3))
(assert/equal (cons '(a b) 'c)         '((a b) . c))
(assert/equal (car '(a b c))           'a)
(assert/equal (car '((a) b c d))       '(a))
(assert/equal (car '(1 . 2))           1)
;(assert/equal (car '())               ==> error)
(assert/equal (cdr '((a) b c d))       '(b c d))
(assert/equal (cdr '(1 . 2))           2)
;(assert/equal (cdr '())               ==>  error)
;(define (f) (list 'not-a-constant-list))
;(define (g) '(constant-list))
;(set-car! (f) 3)                     ===>  unspecified
;(set-car! (g) 3)                     ===>  error
(assert/equal (list? '(a b c))          #t)
(assert/equal (list? '())               #t)
(assert/equal (list? '(a . b))          #f)
;(let ((x (list 'a)))
;      (set-cdr! x x)
;      (list? x))             ===>  #f
(assert/equal (list 'a (+ 3 4) 'c)               '(a 7 c))
(assert/equal (list)                             '())
(assert/equal (length '(a b c))                  3)
(assert/equal (length '(a (b) (c d e)))          3)
(assert/equal (length '())                       0)
(assert/equal (append '(x) '(y))                 '(x y))
(assert/equal (append '(a) '(b c d))             '(a b c d))
(assert/equal (append '(a (b)) '((c)))           '(a (b) (c)))
(assert/equal (append '(a b) '(c . d))           '(a b c . d))
(assert/equal (append '() 'a)                    'a)
(assert/equal (append)                           '())
(assert/equal (append 'x)                        'x)
(assert/equal (append '(1) '(2))                 '(1 2))
(assert/equal (append '(1 2) '(3 4))             '(1 2 3 4))
(assert/equal (append '(1 2) '(3 4) '(5 6 7))    '(1 2 3 4 5 6 7))
(assert/equal (append '(1 2) '(3 4) '(5 6 7) '(8 9 10 11 12 13 14) '(15)) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
(assert/equal (reverse '(a b c))                 '(c b a))
(assert/equal (reverse '(a (b c) d (e (f))))  
                                                  '((e (f)) d (b c) a))

(assert/equal (list-ref '(a b c d) 2)     'c)
(assert/equal (list-ref '(a b c d)
                (inexact->exact (round 1.8))) 
              'c)

(assert/equal (symbol? 'foo)                  #t)
(assert/equal (symbol? (car '(a b)))          #t)
(assert/equal (symbol? "bar")                 #f)
(assert/equal (symbol? 'nil)                  #t)
(assert/equal (symbol? '())                   #f)
(assert/equal (symbol? #f)                    #f)


(assert/equal (symbol->string 'flying-fish)     
               "flying-fish")
(assert/equal (symbol->string 'Martin) 
              "Martin")
(assert/equal (symbol->string
                 (string->symbol "Malvina"))
              "Malvina")
(assert/equal (eq? 'mISSISSIppi 'mississippi)  
                #f)
(assert/equal (string->symbol "mISSISSIppi")  
                'mISSISSIppi)
(assert/equal (eq? 'bitBlt (string->symbol "bitBlt"))     
               #t)
(assert/equal (eq? 'JollyWog
          (string->symbol
                   (symbol->string 'JollyWog)))  
                #t)
(assert/equal (string=? "K. Harper, M.D."
                    (symbol->string
                                  (string->symbol "K. Harper, M.D.")))  
                 #t)

(assert/equal (and (= 2 2) (> 2 1))   #t)
(assert/equal (and (= 2 2) (< 2 1))   #f)
(assert/equal (and 1 2 'c '(f g))     '(f g))
(assert/equal (and)                   #t)

(assert/equal (or (= 2 2) (> 2 1))   #t)
(assert/equal (or (= 2 2) (< 2 1))   #t)
(assert/equal (or #f #f #f)          #f)
(assert/equal (or (memq 'b '(a b c)) 
                  (/ 3 0)) 
              '(b c))

(assert/equal 
    (let ((x 2) (y 3))
          (* x y))
    6)

(assert/equal 
(let ((x 2) (y 3))
    (let ((x 7)
                  (z (+ x y)))
          (* z x))) 
 35)

(assert/equal 
(let ((x 2) (y 3))
    (let* ((x 7)
                    (z (+ x y)))
          (* z x)))
 70)


(define add3
    (lambda (x) (+ x 3)))
(assert/equal (add3 3)
               6)
(define first car)
(assert/equal (first '(1 2))
              1)
(assert/equal
  (let ((x 5))
    (define foo (lambda (y) (bar x y)))
      (define bar (lambda (a b) (+ (* a b) a)))
        (foo (+ x 3)))                        
   45)

; General tests
(assert/equal (+ 1 1) 2)

(assert/equal (map (curry + 2) '(1 2 3 4)) 
			  '(3 4 5 6))

(assert/equal (zero? 0) #t)
(assert/equal (length '(0 1 2 3)) 4)
(assert/equal (member 1 '(1 2 3 4)) '(1 2 3 4))


(assert/equal (memq 'a '(a b c)) '(a b c))
(assert/equal (memq 'b '(a b c)) '(b c))
(assert/equal (memq 'a '(b c d)) #f)
;Not currently supported due to husk memory model: (assert/equal (memq (list 'a) '(b (a) c))) #f)
(assert/equal (member (list 'a) '(b (a) c)) '((a) c))
(assert/equal (memv 101 '(100 101 102)) '(101 102))

(define e '((a 1) (b 2) (c 3)))
(assert/equal (assq 'a e)
                        '(a 1))
(assert/equal (assq 'b e)
                        '(b 2))
(assert/equal (assq 'd e)
                         #f)
;Not currently supported due to husk memory model: (assert/equal (assq (list 'a) '(((a)) ((b)) ((c)))))
;                         #f)
(assert/equal (assoc (list 'a) '(((a)) ((b)) ((c))))   
                        '((a)))
;(assert/equal 
;(assq 5 '((2 3) (5 7) (11 13)))    
;                                   ===>  unspecified
(assert/equal (assv 5 '((2 3) (5 7) (11 13)))    
                        '(5 7))

(assert/equal (list-tail '(a b c d e f g) 5) '(f g))
(assert/equal (list-ref '(a b c d) 2) 'c)

(assert/equal (append '(1 2 3 4 5) '(6 7 "eight" "nine")) 
              '(1 2 3 4 5 6 7 "eight" "nine"))

(define v (make-vector 6))
(assert/equal (for-each (lambda (i)
                                       (vector-set! v i (* i i)))
                              '(0 1 2 3 4 5))
               #(0 1 4 9 16 25))

; Fix for issue #40 - arguments reversed in fold function:
(assert/equal (foldl (lambda (x y) x) 8 (list 1 2 3)) 3)

; See: http://en.wikibooks.org/wiki/Talk:Haskell/Write_Yourself_a_Scheme_in_48_Hours 
; Environment bug:
(define x 3)
(define (f n) (define x n) x)
(assert/equal (f 5) 5)
(assert/equal x 3)
;(f 5) ==> 5 (OK)
;x ==> 5 (BAD, expected it to remain 3)

(define (test2) #f)
(define (test)
  1
  (test2)
  (null? '())
  2
  4
  #f)
(assert/equal (if (test) 'true 'false) 'false)

(unit-test-handler-results)
