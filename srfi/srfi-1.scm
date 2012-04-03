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


; TODO: grabbed this helper from SRFI-3, not sure if it is compatible
;;;    (define (check-arg pred val caller)
;;;      (let lp ((val val))
;;;        (if (pred val) val (lp (error "Bad argument" val pred caller)))))


;; Constructors
(define (xcons d a) (cons a d))

; TODO:
;(define (make-list len . maybe-elt)
;  (check-arg (lambda (n) (and (integer? n) (>= n 0))) len make-list)
;    (let ((elt (cond ((null? maybe-elt) #f) ; Default value
;               ((null? (cdr maybe-elt)) (car maybe-elt))
;                       (else (error "Too many arguments to MAKE-LIST"
;                                    (cons len maybe-elt))))))
;        (do ((i len (- i 1))
;             (ans '() (cons elt ans)))
;            ((<= i 0) ans))))

(define (cons* first . rest)
  (let recur ((x first) (rest rest))
      (if (pair? rest)
          (cons x (recur (car rest) (cdr rest)))
          x)))

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
