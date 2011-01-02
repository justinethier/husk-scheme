(load "skim-unit.scm")

; TODO: commented out lines indicate issue with Core
;(define (f) (make-string 3 #\*))
(define f (make-string 3 #\*))
(define (g) "***")
(string-set! f 0 #\?)
;(string-set! (f) 0 #\?)          ===>  unspecified
(assert/equal f "?**")
;(string-set! (g) 0 #\?)          ===>  error
;(string-set! (symbol->string 'immutable)
;                          0
;                                       #\?)          ===>  error
(unit-test-handler-results)
