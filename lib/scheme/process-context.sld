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
         system
         exit-success
         exit-fail
; TODO:
;        command-line
;        emergency-exit
;        exit
;        get-environment-variable
;        get-environment-variables
        )
    (import (scheme)))
