;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; r7rs-small r5rs library
;;;

(define-library (scheme r5rs)


; TODO: this is broken somehow, goes into a 100% CPU loop
;       when imported by the REPL
    (export-all)
    (import (scheme r5rs)))
