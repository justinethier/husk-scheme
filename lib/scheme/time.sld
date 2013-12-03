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
    (scheme base)
    (scheme time posix))
  (begin
    (define (jiffies-per-second) 10000)
    (define (current-jiffy)
      (exact (* (jiffies-per-second) (current-second))))))
