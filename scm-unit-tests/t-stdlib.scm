;
; Unit tests for the standard library
;
(load "skim-unit.scm")


; 
; TODO:
;
; start here, and go through the spec, adding missing test cases:
; http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_chap_4
;
;

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
; TODO:
; Looks like (need to confirm) all of below fail because (define y x) 
; is assigning to to the evaluated value of x instead of the symbol x.
;;(assert/equal (eqv? x y) #t)
;;(assert/equal y '(a . 4))
;;(assert/equal (list? y) #f)
; End test failures
(set-cdr! x x)
(assert/equal (list? x)  #f)

(assert/equal (pair? (list 1 2 3)) #t)
(assert/equal (pair? 1) #f)
(assert/equal (pair? '(a . b)) #t)
(assert/equal (pair? '(a b c)) #t)
(assert/equal (pair? '()) #f)
(assert/equal (pair? '#(a b)) #f)


(assert/equal (and (= 2 2) (> 2 1))   #t)
(assert/equal (and (= 2 2) (< 2 1))   #f)
;TODO: test from spec
;(assert/equal (and 1 2 'c '(f g))     '(f g))
(assert/equal (and)                   #t)

(assert/equal (or (= 2 2) (> 2 1))   #t)
(assert/equal (or (= 2 2) (< 2 1))   #t)
(assert/equal (or #f #f #f)          #f)
; TODO: test from spec
;(assert/equal (or (memq 'b '(a b c)) 
;                  (/ 3 0)) 
;              '(b c))

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



; General tests
(assert/equal (+ 1 1) 2)

(assert/equal (map (curry + 2) '(1 2 3 4)) 
			  '(3 4 5 6))

(assert/equal (filter even? '(1 2 3 4)) 
			  '(2 4))

(assert/equal (zero? 0) #t)
(assert/equal (length '(0 1 2 3)) 4)
(assert/equal (member 1 '(1 2 3 4)) '(1 2 3 4))


(assert/equal (memq 'a '(a b c)) '(a b c))
(assert/equal (memq 'b '(a b c)) '(b c))
(assert/equal (memq 'a '(b c d)) #f)
;TODO: (assert/equal (memq (list 'a) '(b (a) c))) #f)
(assert/equal (member (list 'a) '(b (a) c)) '((a) c))
(assert/equal (memv 101 '(100 101 102)) '(101 102))

(define e '((a 1) (b 2) (c 3)))
(assert/equal (assq 'a e)
                        '(a 1))
(assert/equal (assq 'b e)
                        '(b 2))
(assert/equal (assq 'd e)
                         #f)
;TODO: (assert/equal (assq (list 'a) '(((a)) ((b)) ((c)))))
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
      (write (+ 1 2 3))
        4
          (write (+ 4 5 6))
            #f)
(assert/equal (if (test) 'true 'false) 'false)

(unit-test-handler-results)
