;;;
;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;;
;;; Most of this code was taken from the reference implementation 
;;; at http://srfi.schemers.org/srfi-1/srfi-1-reference.scm
;;; Copyright (c) 1998, 1999 by Olin Shivers.
;;;
;;;
;;; Implementation of SRFI-1: List Library
;;;
;;; This file contains all of the SRFI-1 functions
;;; that are not part of the standard library.
;;;


; TODO: add srfi file(s) to the installation and
; add a simplified version of (import) that will 
; just take the name of the SRFI and (load) that
; srfi file. No need to implement full module/library
; semantics just yet. But this will be a first step...
;
; A simple way to do this is via SRFI 55's require-extension:
; http://srfi.schemers.org/srfi-55/srfi-55.html
;

;; Helpers (TODO: should be private to this "module")
(define (check-arg pred val caller)
  (let lp ((val val))
    (if (pred val) val (lp (error "Bad argument" val pred caller)))))


;; Constructors

;;; Occasionally useful as a value to be passed to a fold or other
;;; higher-order procedure.
(define (xcons d a) (cons a d))

;;; Make a list of length LEN.
(define (make-list len . maybe-elt)
  (check-arg (lambda (n) (and (integer? n) (>= n 0))) len make-list)
    (let ((elt (cond ((null? maybe-elt) #f) ; Default value
               ((null? (cdr maybe-elt)) (car maybe-elt))
                       (else (error "Too many arguments to MAKE-LIST"
                                    (cons len maybe-elt))))))
        (do ((i len (- i 1))
             (ans '() (cons elt ans)))
            ((<= i 0) ans))))

;;; Make a list of length LEN. Elt i is (PROC i) for 0 <= i < LEN.
(define (list-tabulate len proc)
    (check-arg (lambda (n) (and (integer? n) (>= n 0))) len list-tabulate)
    (check-arg procedure? proc list-tabulate)
    (do ((i (- len 1) (- i 1))
         (ans '() (cons (proc i) ans)))
        ((< i 0) ans)))

;;; Create a copy of a list
(define (list-copy lis)             
    (let recur ((lis lis))          
        (if (pair? lis)               
            (cons (car lis) (recur (cdr lis)))  
            lis)))  

;;; Like list, but the last argument provides the tail of the constructed list
(define (cons* first . rest)
  (let recur ((x first) (rest rest))
      (if (pair? rest)
          (cons x (recur (car rest) (cdr rest)))
          x)))

; TODO:
;(define (circular-list val1 . vals)
;    (let ((ans (cons val1 vals)))
;        (set-cdr! (last-pair ans) ans)
;        ans))

; TODO:
;(define (iota count . maybe-start+step)
;    (check-arg integer? count iota)
;    (if (< count 0) (error "Negative step count" iota count))
;    (let-optionals maybe-start+step ((start 0) (step 1))
;        (check-arg number? start iota)
;        (check-arg number? step iota)
;        (let loop ((n 0) (r '()))
;            (if (= n count)
;                (reverse r)
;                (loop (+ 1 n)
;                    (cons (+ start (* n step)) r))))))


;; Predicates
(define (proper-list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
	(let ((x (cdr x)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag)))
		(and (not (eq? x lag)) (lp x lag)))
	      (null? x)))
	(null? x))))


;;; A dotted list is a finite list (possibly of length 0) terminated
;;; by a non-nil value. Any non-cons, non-nil value (e.g., "foo" or 5)
;;; is a dotted list of length 0.
;;;
;;; <dotted-list> ::= <non-nil,non-pair>	; Empty dotted list
;;;               |   (cons <x> <dotted-list>)	; Proper-list pair

(define (dotted-list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
	(let ((x (cdr x)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag)))
		(and (not (eq? x lag)) (lp x lag)))
	      (not (null? x))))
	(not (null? x)))))

(define (circular-list? x)
  (let lp ((x x) (lag x))
    (and (pair? x)
	 (let ((x (cdr x)))
	   (and (pair? x)
		(let ((x   (cdr x))
		      (lag (cdr lag)))
		  (or (eq? x lag) (lp x lag))))))))

(define (not-pair? x) (not (pair? x)))

;;; This is a legal definition which is fast and sloppy:
;;;     (define null-list? not-pair?)
;;; but we'll provide a more careful one:
(define (null-list? l)
  (cond ((pair? l) #f)
	((null? l) #t)
	(else (error "null-list?: argument out of domain" l))))
           
(define (list= = . lists)
  (or (null? lists) ; special case

      (let lp1 ((list-a (car lists)) (others (cdr lists)))
	(or (null? others)
	    (let ((list-b (car others))
		  (others (cdr others)))
	      (if (eq? list-a list-b)	; EQ? => LIST=
		  (lp1 list-b others)
		  (let lp2 ((list-a list-a) (list-b list-b))
		    (if (null-list? list-a)
			(and (null-list? list-b)
			     (lp1 list-b others))
			(and (not (null-list? list-b))
			     (= (car list-a) (car list-b))
			     (lp2 (cdr list-a) (cdr list-b)))))))))))

;; Selectors
(define first  car)
(define second cadr)
(define third  caddr)
(define fourth cadddr)
(define (fifth   x) (car    (cddddr x)))
(define (sixth   x) (cadr   (cddddr x)))
(define (seventh x) (caddr  (cddddr x)))
(define (eighth  x) (cadddr (cddddr x)))
(define (ninth   x) (car  (cddddr (cddddr x))))
(define (tenth   x) (cadr (cddddr (cddddr x))))

(define (car+cdr pair) (values (car pair) (cdr pair)))

;;; take & drop

(define (take lis k)
  (check-arg integer? k take)
  (let recur ((lis lis) (k k))
    (if (zero? k) '()
	(cons (car lis)
	      (recur (cdr lis) (- k 1))))))

(define (drop lis k)
  (check-arg integer? k drop)
  (let iter ((lis lis) (k k))
    (if (zero? k) lis (iter (cdr lis) (- k 1)))))

(define (take! lis k)
  (check-arg integer? k take!)
  (if (zero? k) '()
      (begin (set-cdr! (drop lis (- k 1)) '())
	     lis)))

;;; TAKE-RIGHT and DROP-RIGHT work by getting two pointers into the list, 
;;; off by K, then chasing down the list until the lead pointer falls off
;;; the end.

(define (take-right lis k)
  (check-arg integer? k take-right)
  (let lp ((lag lis)  (lead (drop lis k)))
    (if (pair? lead)
	(lp (cdr lag) (cdr lead))
	lag)))

(define (drop-right lis k)
  (check-arg integer? k drop-right)
  (let recur ((lag lis) (lead (drop lis k)))
    (if (pair? lead)
	(cons (car lag) (recur (cdr lag) (cdr lead)))
	'())))

;;; In this function, LEAD is actually K+1 ahead of LAG. This lets
;;; us stop LAG one step early, in time to smash its cdr to ().
(define (drop-right! lis k)
  (check-arg integer? k drop-right!)
  (let ((lead (drop lis k)))
    (if (pair? lead)

	(let lp ((lag lis)  (lead (cdr lead)))	; Standard case
	  (if (pair? lead)
	      (lp (cdr lag) (cdr lead))
	      (begin (set-cdr! lag '())
		     lis)))

	'())))	; Special case dropping everything -- no cons to side-effect.

; TODO:
;(define (split-at x k)
;  (check-arg integer? k split-at)
;  (let recur ((lis x) (k k))
;    (if (zero? k) (values '() lis)
;	(receive (prefix suffix) (recur (cdr lis) (- k 1))
;	  (values (cons (car lis) prefix) suffix)))))

(define (split-at! x k)
  (check-arg integer? k split-at!)
  (if (zero? k) (values '() x)
      (let* ((prev (drop x (- k 1)))
	     (suffix (cdr prev)))
	(set-cdr! prev '())
	(values x suffix))))


(define (last lis) (car (last-pair lis)))

(define (last-pair lis)
  (check-arg pair? lis last-pair)
  (let lp ((lis lis))
    (let ((tail (cdr lis)))
      (if (pair? tail) (lp tail) lis))))

;; Misc

(define (length+ x)			; Returns #f if X is circular.
  (let lp ((x x) (lag x) (len 0))
    (if (pair? x)
	(let ((x (cdr x))
	      (len (+ len 1)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag))
		    (len (+ len 1)))
		(and (not (eq? x lag)) (lp x lag len)))
	      len))
	len)))

