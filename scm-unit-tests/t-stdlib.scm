;
; Unit tests for the standard library
;
(load "skim-unit.scm")

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

(unit-test-handler-results)
