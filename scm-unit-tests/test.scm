; TODO: this will become another set of macro test cases, but for now I
; just want to get it into git... we'll pretty it up later

;(define-syntax nesting-test2
;  (syntax-rules ()
;    ((test a b ...)
;     (quote (a b ...)))))
;(write (nesting-test2 1 2 5 6 7 8 9 10))

; This macro demonstrates multi-level nesting of macros
; It actually crashes v3.2 husk because that version does not properly
; handle nesting
(define-syntax nesting-test
  (syntax-rules ()
    ((test a b (c d (e f ...) ...) ...)
     '(a b ((e f ...) ...) ...))))
(write (nesting-test 1 2 ))
(write (nesting-test 1 2 (3 4 (5 6 7 8 9 10))))
