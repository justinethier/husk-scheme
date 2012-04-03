;;;
;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
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
(define (not-pair? x) (not (pair? x)))

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
