;;;
;;; Justin Ethier
;;; husk-scheme
;;;
;;; Example program: Greatest common denominator
;;;
;;; Based on example code from SCIP. The real reason to show this program
;;; is to demonstrate that a (main) function can be defined in order to 
;;; enable a program to receive command line arguments.
;;;
;;; For example:
;;;
;;;  ./huski examples/gcd-w-cmd-line-args.scm 42 4
;;;  2
;;;
(load "stdlib.scm")

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (main args)
  (write (gcd
    (string->number (list-ref args 1))
    (string->number (list-ref args 2)))))
