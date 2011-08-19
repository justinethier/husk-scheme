#| original macro:
(define-syntax when
(syntax-rules ()
((when condition . body) (if condition (begin . body) #f))))

(when (negative? -1) (newline) (display "bad number: negative"))
|#

; test macro to investigate how pairs are supposed to be handled
(define-syntax test
  (syntax-rules ()
     ((_ (a b . c))
      (list (quote (a b . c))))))

(write 
(test (1 2 3 . 3)))

(write 
(test (1 2)))

(write
(test (1 2 . 3)))

(define-syntax nesting-test2
  (syntax-rules ()
    ((test a b ...)
     (quote (a b ...)))))
(write (nesting-test2 1 2 5 6 7 8 9 10))
; This macro demonstrates multi-level nesting of macros
; It actually crashes v3.2 husk because that version does not properly
; handle nesting
(define-syntax nesting-test
  (syntax-rules ()
    ((test a b (c d (e f ...) ...) ...)
     (quote ((e f ...) ...) ...))))
(write (nesting-test 1 2 (3 4 (5 6 7 8 9 10))))
