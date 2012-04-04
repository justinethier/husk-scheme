;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for SRFI-1
;;
(unit-test-start "SRFI 1: List Library")

; TODO: (import srfi-1)
; Note - can test these in chicken using: (require-extension srfi-1)

(load "../srfi/srfi-1.scm")

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
; TODO: car+cdr, take!, drop-right!, split-at!
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



; TODO: add a list here of all functions that are untested


(unit-test-handler-results)
