;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; The r7rs time library
;;;

(define-library (scheme time)
  (export
    current-second
    current-jiffy
    jiffies-per-second)
  (import 
    (scheme r5rs) ; TODO: use proper r7rs library for inexact->exact!!!!!
    (scheme time posix))
  (begin
    (define (jiffies-per-second) 10000)
    (define (current-jiffy)
      (inexact->exact (* (jiffies-per-second) (current-second))))))
