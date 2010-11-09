;
; Unit tests for the skim standard library
;
(load "skim-unit.scm")

(assert/equal (+ 1 1) 2)

(assert-equal (lambda () (map (curry + 2) '(1 2 3 4))) 
			  '(3 4 5 6))

(assert-equal (lambda () (filter even? '(1 2 3 4))) 
			  '(2 4))

(assert-equal (lambda () (zero? 0)) #t)
(assert-equal (lambda () (length '(0 1 2 3))) 4)
(assert-equal (lambda () (member 1 '(1 2 3 4))) '(1 2 3 4))


(assert-equal (lambda () (memq 'a '(a b c))) '(a b c))
(assert-equal (lambda () (memq 'b '(a b c))) '(b c))
(assert-equal (lambda () (memq 'a '(b c d))) #f)
;TODO: (assert-equal (lambda () (memq (list 'a) '(b (a) c))) #f)
(assert-equal (lambda () (member (list 'a) '(b (a) c))) '((a) c))
(assert-equal (lambda () (memv 101 '(100 101 102))) '(101 102))

(define e '((a 1) (b 2) (c 3)))
(assert-equal (lambda () (assq 'a e))
                        '(a 1))
(assert-equal (lambda () (assq 'b e))
                        '(b 2))
(assert-equal (lambda () (assq 'd e))
                         #f)
;TODO: (assert-equal (lambda () (assq (list 'a) '(((a)) ((b)) ((c)))))
;                         #f)
(assert-equal (lambda () (assoc (list 'a) '(((a)) ((b)) ((c)))))   
                        '((a)))
;(assert-equal (lambda () 
;(assq 5 '((2 3) (5 7) (11 13)))    
;                                   ===>  unspecified
(assert-equal (lambda () (assv 5 '((2 3) (5 7) (11 13))))    
                        '(5 7))

(assert-equal (lambda () (list-tail '(a b c d e f g) 5)) '(f g))
(assert-equal (lambda () (list-ref '(a b c d) 2)) 'c)

(assert-equal (lambda () (append '(1 2 3 4 5) '(6 7 "eight" "nine"))) 
              '(1 2 3 4 5 6 7 "eight" "nine"))

(define v (make-vector 6))
(assert-equal (lambda () (for-each (lambda (i)
                                       (vector-set! v i (* i i)))
                              '(0 1 2 3 4 5)))
               #(0 1 4 9 16 25))

(unit-test-handler-results)
