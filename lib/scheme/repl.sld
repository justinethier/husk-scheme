;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; r7rs-small repl library
;;;

(define-library (scheme repl)
    (export
        interaction-environment)
    (import (scheme)))
