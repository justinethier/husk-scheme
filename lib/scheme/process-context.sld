;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; r7rs process-context library
;;;

(define-library (scheme process-context)
    (export 
         emergency-exit
         get-environment-variable
         get-environment-variables
         exit-fail
         exit-success
         system)
    (import (scheme))
    (begin
        (define (get-environment-variable var)
          (let ((var+val (assoc var (get-environment-variables))))
            (if var+val
                (cdr var+val)
                #f)))
        (define (emergency-exit . obj)
            (if (or (null? obj)
                    (car obj))
                (exit-success)
                (exit-fail)))))
