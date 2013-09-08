;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; r7rs eval library
;;;

(define-library (scheme eval)
    (export 
        eval
        ; environment
        ; TODO: not in r7rs, not sure proper place => current-environment
        ; TODO: this in in REPL - interaction-environment
        ; TODO: not in r7rs, not sure proper place => make-environment
        ; TODO: not in r7rs, not sure proper place => null-environment
        )
    (import (scheme)))
