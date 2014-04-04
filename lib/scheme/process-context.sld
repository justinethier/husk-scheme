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
         get-environment-variables
; TODO:
;        command-line
;        exit
;        get-environment-variable
         exit-fail
         exit-success
         system)
    (import (scheme))
    (begin
        (define (emergency-exit . obj)
            (if (or (null? obj)
                    (car obj))
                (exit-success)
                (exit-fail)))))
