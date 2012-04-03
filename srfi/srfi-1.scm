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


(lambda (d a) (cons a d))

(define (cons* first . rest)
  (let recur ((x first) (rest rest))
      (if (pair? rest)
          (cons x (recur (car rest) (cdr rest)))
          x)))

