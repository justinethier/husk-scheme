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

; Constructors
(assert/equal (cons* 1 2 3 4) '(1 2 3 . 4))
(assert/equal (cons* 1) 1)
(assert/equal (make-list 4 'c) '(c c c c))
(assert/equal (list-tabulate 4 values) '(0 1 2 3))
(assert/equal (list-copy (list 1 2 3 4.4 "5")) (list 1 2 3 4.4 "5"))
; TODO: (circular-list 'z 'q) => (z q z q z q ...)
; TODO: need to replace let-optional in iota:
; TODO: (assert/equal (iota 5) '(0 1 2 3 4))
; TODO: (assert/equal (iota 5 0 -0.1) '(0 -0.1 -0.2 -0.3 -0.4))

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
; TODO: take!, drop-right!, split-at!
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
; TODO: (take! (circular-list 1 3 5) 8) => (1 3)
; TODO: (take! (circular-list 1 3 5) 8) => (1 3 5 1 3 5 1 3)

(assert/equal (split-at '(a b c d e f g h) 3) '(a b c))
(assert/equal (last '(a b c)) 'c)
(assert/equal (last-pair '(a b c)) '(c))

; Misc
(assert/equal (length+ '(a b c)) 3)
; TODO: append! concatenate! reverse! append-reverse!
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


; TODO:
;
; Fold / Unfold / Map
;TODO: fold       unfold       pair-fold       reduce 
;TODO: fold-right unfold-right pair-fold-right reduce-right 
;TODO: append-map append-map!
;TODO: map! pair-for-each filter-map map-in-order

; Filtering and partitioning
;TODO: filter  partition  remove
;TODO: filter! partition! remove! 

; Searching
; TODO: find find-tail 
; TODO: any every
; TODO: list-index
; TODO: take-while drop-while take-while!
; TODO: span break span! break!

; Deleting
;TODO: delete
;TODO: delete! delete-duplicates!
(assert/equal (delete-duplicates '(a b a c a b c z)) '(a b c z))
(assert/equal (delete-duplicates '((a . 3) (b . 7) (a . 9) (c . 1))
                   (lambda (x y) (eq? (car x) (car y))))
    '((a . 3) (b . 7) (c . 1)))

; Association lists
;TODO: alist-cons
;TODO: alist-copy
;TODO: alist-delete 
;TODO: alist-delete!

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

; TODO: lset-diff+intersection
; TODO: lset-union!       
; TODO: lset-intersection!
; TODO: lset-difference!  
; TODO: lset-xor!         
; TODO: lset-diff+intersection!

(unit-test-handler-results)
