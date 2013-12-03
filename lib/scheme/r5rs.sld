;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; r7rs-small r5rs library
;;;

; TODO: this could be much more precise, and 
;       not include the r7rs stuff...
(define-library (scheme r5rs)
    (export-all)
    (import (scheme)))
