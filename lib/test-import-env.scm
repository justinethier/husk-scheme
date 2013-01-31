; Simple test to prove if %import into env is working
; A temporary program; remove once this is working
(define env (make-environment))
(write (list 'initial: (print-env env)))
(%import env (current-environment) #f #f)
(write (list 'final: (print-env env)))
