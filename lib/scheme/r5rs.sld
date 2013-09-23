;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; r7rs-small r5rs library
;;;


; TODO: well obviously this won't work, since it includes itself
; need a way to load r5rsEnv on demand. unfortunately the direct bindings
; null-env, etc are supposed to be in r5rs. so may need a  husk-specific
; function to do it...
;
;(define-library (scheme r5rs)
;    (export-all)
;    (import (scheme r5rs)))
