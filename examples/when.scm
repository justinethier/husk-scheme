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
(test (1 2)))

(write 
(test (1 2 3 . 3)))

(write
(test (1 2 3)))
