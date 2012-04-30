;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for SRFI-1
;;
(unit-test-start "SRFI 1: List Library")
(require-extension (srfi 1))

; TODO: linear update functions will not be supported now:
; take!, drop-right!, split-at!
; append! concatenate! reverse! append-reverse!
; filter! partition! remove! 
; take-while! span! break!
; delete! delete-duplicates!
; alist-delete!
; lset-union!       
; lset-intersection!
; lset-difference!  
; lset-xor!         
; lset-diff+intersection!
; map! append-map!

; Constructors
(assert/equal (cons* 1 2 3 4) '(1 2 3 . 4))
(assert/equal (cons* 1) 1)
(assert/equal (make-list 4 'c) '(c c c c))
(assert/equal (list-tabulate 4 values) '(0 1 2 3))
(assert/equal (list-copy (list 1 2 3 4.4 "5")) (list 1 2 3 4.4 "5"))
; TODO: (circular-list 'z 'q) => (z q z q z q ...)
(assert/equal (iota 5) '(0 1 2 3 4))
(assert/equal (iota 5 0 -0.5) '(0.0 -0.5 -1.0 -1.5 -2.0))

; Predicates
(assert/equal (proper-list? (list))       #t)
(assert/equal (proper-list? '(1 . 2))      #f)
;TODO: circular-list? with a true result
(assert/equal (circular-list? (list))       #f)
(assert/equal (dotted-list? (list))       #f)
(assert/equal (dotted-list? '(1 . 2))     #t)
(assert/equal (null-list? '())        #t)
(assert/equal (not-pair? '(1 . 2))    #f)
(assert/equal (list= eq?)             #t)       ; Trivial cases
(assert/equal (list= eq? '(a))        #t)

; Selectors
(assert/equal (car+cdr '(a b c d)) 'a)
(assert/equal (first '(a b c d)) 'a)
(assert/equal (take '(a b c d e)  2) '(a b))
(assert/equal (drop '(a b c d e)  2) '(c d e))
(assert/equal (take '(1 2 3 . d) 2)  '(1 2))
(assert/equal (drop '(1 2 3 . d) 2)  '(3 . d))
(assert/equal (take '(1 2 3 . d) 3)  '(1 2 3))
(assert/equal (drop '(1 2 3 . d) 3)  'd)
;For a legal i, take and drop partition the list in a manner which can be inverted with append:
(assert/equal (append (take (list 1 2 3 4 5 6) 3) (drop (list 1 2 3 4 5 6) 3)) (list 1 2 3 4 5 6))

(assert/equal (take-right '(a b c d e) 2) '(d e))
(assert/equal (drop-right '(a b c d e) 2) '(a b c))
(assert/equal (take-right '(1 2 3 . d) 2) '(2 3 . d))
(assert/equal (drop-right '(1 2 3 . d) 2) '(1))
(assert/equal (take-right '(1 2 3 . d) 0) 'd)
(assert/equal (drop-right '(1 2 3 . d) 0) '(1 2 3))
;For a legal i, take-right and drop-right partition the list in a manner which can be inverted with append:
(let ((flist (list 1 2 3 4 5 6))
      (i 2))
    (assert/equal (append (take flist i) (drop flist i)) flist))

(assert/equal (split-at '(a b c d e f g h) 3) '(a b c))
(assert/equal (last '(a b c)) 'c)
(assert/equal (last-pair '(a b c)) '(c))

; Misc
(assert/equal (length+ '(a b c)) 3)
(assert/equal (concatenate '((1) (2) (3) (4) (5))) '(1 2 3 4 5)) 
(assert/equal (append-reverse '(4 3 2 1) '(5)) '(1 2 3 4 5)) 
(assert/equal (append-reverse '(4 3 2 1) '(5)) (append (reverse '(4 3 2 1)) '(5)))

; TODO:
;(assert/equal (zip '(one two three) 
;                   '(1 2 3)
;                   '(odd even odd even odd even odd even))
;             '((one 1 odd) (two 2 even) (three 3 odd)))
;
;(assert/equal (zip '(1 2 3)) '((1) (2) (3)))
;(assert/equal (zip '(3 1 4 1) (circular-list #f #t)) 
;             '((3 #f) (1 #t) (4 #f) (1 #t)))
(assert/equal (unzip2 '((1 one) (2 two) (3 three)))
             '(1 2 3))
(assert/equal (count even? '(3 1 4 1 5 9 2 5 6)) 3)
;TODO: (assert/equal (count < '(1 2 4 8) '(2 4 6 8 10 12 14 16)) 3)
;TODO: (assert/equal (count < '(3 1 4 1) (circular-list 1 10)) 2)


; Fold / Unfold / Map
(assert/equal (fold + 0 '(1 2 3 4)) (+ 1 2 3 4))
(assert/equal (unfold null-list? car cdr '(a b c)) '(a b c))
(assert/equal (unfold null-list? car cdr '(1)
                (lambda (x) '(2 3 4)))
              '(1 2 3 4))
(assert/equal (reduce + 0 '(1 2 3 4)) (+ 1 2 3 4))
(assert/equal
    (pair-fold-right cons '() '(a b c))
   '((a b c) (b c) (c)))
(assert/equal 
    (unfold-right null-list? car cdr '(9 8 7 6 5 4 3 2 1))
   '(1 2 3 4 5 6 7 8 9))
(assert/equal 
    (reduce-right append '() '((1) (2) (3)))
   '(1 2 3))
(assert/equal 
    (fold-right cons '() '(a b c))
   '(a b c)) ;; Copy LIS.
(assert/equal 
    (fold-right (lambda (x l) (if (even? x) (cons x l) l)) '() '(1 2 3 4))
   '(2 4)) ;; Filter the even numbers out of LIS.
(assert/equal 
    (fold-right cons* '() '(a b c) '(1 2 3 4 5))
   '(a 1 b 2 c 3))
(assert/equal
    (append-map (lambda (x) (list x (- x))) '(1 3 8))
   '(1 -1 3 -3 8 -8))
(assert/equal
    (filter-map (lambda (x) (and (number? x) (* x x))) '(a 1 b 3 c 7))
   '(1 9 49))
(assert/equal 
    (map-in-order cadr '((a b) (d e) (g h)))
   '(b e h))
(assert/equal
    (map-in-order (lambda (n) (inexact->exact (expt n n)))
        '(1 2 3 4 5))
   '(1 4 27 256 3125))
(assert/equal
    (map-in-order + '(1 2 3) '(4 5 6))
   '(5 7 9))
(assert/equal
    (let ((count 0))
      (map (lambda (ignored)
               (set! count (+ count 1))
                        count)
             '(a b)))
   '(1 2))
(let ((tmp '()))
    (pair-for-each (lambda (pair) (set! tmp (append tmp pair))) '(a b c))
    (assert/equal tmp '(a b c b c c)))


; TODO: this illustrates a bug in husk, since the result is not simplified to (3 2 1)
;(assert/equal
;    (pair-fold (lambda (pair tail) (set-cdr! pair tail) pair) '() '(1 2 3))
;   '(3 . (2 . (1 . ()))))
(assert/equal #t #f)


; Filtering and partitioning
(assert/equal (filter even? '(1 2 3 4)) 
			  '(2 4))
(assert/equal (filter even? '(0 7 8 8 43 -4)) '(0 8 8 -4))
(assert/equal (partition symbol? '(one 2 3 four five 6))
             '(one four five))
(assert/equal  (remove even? '(0 7 8 8 43 -4)) '(7 43))

; Searching
;; Proper list -- success
(assert/equal (find even? '(1 2 3)) 2)
(assert/equal (find even? '(1 2 3 4)) 2)
(assert/equal (find even? '(3 1 4 1 5 9)) 4)
(assert/equal (any  even? '(1 2 3)) #t)
;; proper list -- failure
(assert/equal (find even? '(1 7 3)) #f)
(assert/equal (any  even? '(1 7 3)) #f)
(assert/equal (find-tail even? '(3 1 37 -8 -5 0 0)) '(-8 -5 0 0))
(assert/equal (find-tail even? '(3 1 37 -5)) #f)
(assert/equal (every even? (list 2 4)) #t)
(assert/equal (every even? (list 2 5)) #f)
(assert/equal (list-index even? '(3 1 4 1 5 9)) 2)
(assert/equal (list-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)) 1)
(assert/equal (list-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)) #f)
(assert/equal (take-while even? '(2 18 3 10 22 9)) '(2 18))
(assert/equal (drop-while even? '(2 18 3 10 22 9)) '(3 10 22 9))
(assert/equal (span even? '(2 18 3 10 22 9)) '(2 18))
(assert/equal (break even? '(3 1 4 1 5 9)) '(3 1))

; Deleting
(assert/equal (delete 1 (list 1 2 1 3 'a)) '(2 3 a))
(assert/equal (delete-duplicates '(a b a c a b c z)) '(a b c z))
(assert/equal (delete-duplicates '((a . 3) (b . 7) (a . 9) (c . 1))
                   (lambda (x y) (eq? (car x) (car y))))
    '((a . 3) (b . 7) (c . 1)))

; Association lists
(define e '((a 1) (b 2) (c 3)))
(assert/equal (assq 'a e)                             '(a 1))
(assert/equal (assq 'b e)                             '(b 2))
(assert/equal (assq 'd e)                             '#f)
;TODO: (assert/equal (assq (list 'a) '(((a)) ((b)) ((c))))   '#f)
(assert/equal (assoc (list 'a) '(((a)) ((b)) ((c))))  '((a)))
(assert/equal (assv 5 '((2 3) (5 7) (11 13)))    '(5 7))

(assert/equal (alist-cons 'a 0 (list '(1 . 2) '(3 4))) '((a . 0) (1 . 2) (3 4)))
(assert/equal (alist-copy (list '(1 2) '(3 4))) '((1 2) (3 4)))
(assert/equal (alist-delete 1 '((1 . 2))) '())

; Set operations on lists
(assert/equal #t (lset<= eq? '(a) '(a b a) '(a b c c)))
(assert/equal #t (lset<= eq?))
(assert/equal #t (lset<= eq? '(a)))
(assert/equal (lset= eq? '(b e a) '(a e b) '(e e b a))  #t)
(assert/equal (lset= eq?) #t)
(assert/equal (lset= eq? '(a)) #t)
(assert/equal (lset-adjoin eq? '(a b c d c e) 'a 'e 'i 'o 'u) '(u o i a b c d c e))
(assert/equal (lset-union eq? '(a b c d e) '(a e i o u)) '(u o i a b c d e))
;; Repeated elements in LIST1 are preserved.
(assert/equal (lset-union eq? '(a a c) '(x a x)) '(x a a c))
;; Trivial cases
(assert/equal (lset-union eq?) '())
(assert/equal (lset-union eq? '(a b c)) '(a b c))
(assert/equal (lset-intersection eq? '(a b c d e) '(a e i o u)) '(a e))
;; Repeated elements in LIST1 are preserved.
(assert/equal (lset-intersection eq? '(a x y a) '(x a x z)) '(a x a))
(assert/equal (lset-intersection eq? '(a b c)) '(a b c))     ; Trivial case
(assert/equal (lset-difference eq? '(a b c d e) '(a e i o u)) '(b c d))
(assert/equal (lset-difference eq? '(a b c)) '(a b c)) ; Trivial case
(assert/equal (lset-xor eq? '(a b c d e) '(a e i o u)) (reverse '(d c b i o u)))
(assert/equal (lset-xor eq?) '())
(assert/equal (lset-xor eq? '(a b c d e)) '(a b c d e))

(unit-test-handler-results)
