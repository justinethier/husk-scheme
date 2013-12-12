;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; r7rs-small case-lambda library
;;;


; TODO: this is the macro from the spec, but husk
; does not handle the let-syntax portion...

(define-library (scheme case-lambda)
    (export
        case-lambda)
    (import (scheme base))
    (begin
        (define-syntax case-lambda
          (syntax-rules ()
            ((case-lambda (params body0 ...) ...)
             (lambda args
              (let ((len (length args)))
                (let-syntax
                    ((cl (syntax-rules ::: ()
                           ((cl)
                            (error "no matching clause"))
                           ((cl ((p :::) body :::) rest :::)
                           ;((cl ((p :::) . body) . rest)
                            (if (= len (length '(p :::)))
                                (apply (lambda (p :::)
                                       body :::)
                                       ;. body)
                                   args)
                                (cl rest :::)))
                                ;(cl . rest)))
                           ; TODO: for now, var-length arg support is broken
                           ;((cl ((p ::: . tail) . body)
                           ;     . rest)
                           ; (if (>= len (length '(p :::)))
                           ;    (apply
                           ;     (lambda (p ::: . tail)
                           ;        . body)
                           ;     args)
                           ;    (cl . rest)))
                           )))
                    (cl (params body0 ...) ...))))))) 
    ))
