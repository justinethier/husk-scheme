; TODO:
define (f) (make-string 3 #\*))
(define (g) "***")
(string-set! (f) 0 #\?)          ===>  unspecified
(string-set! (g) 0 #\?)          ===>  error
(string-set! (symbol->string 'immutable)
                          0
                                       #\?)          ===>  error
