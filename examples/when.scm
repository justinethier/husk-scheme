(define-syntax when
(syntax-rules ()
((when condition . body) (if condition (begin . body) #f))))

(when (negative? -1) (newline) (display "bad number: negative"))
