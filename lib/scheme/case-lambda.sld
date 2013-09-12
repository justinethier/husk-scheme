;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; r7rs-small case-lambda library
;;;

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
                    ((cl ((p :::) . body) . rest)
                     (if (= len (length '(p :::)))
                         (apply (lambda (p :::)
                                . body)
                            args)
                         (cl . rest)))
                    ((cl ((p ::: . tail) . body)
                         . rest)
                     (if (>= len (length '(p :::)))
                        (apply
                         (lambda (p ::: . tail)
                            . body)
                         args)
                        (cl . rest))))))
                    (cl (params body0 ...) ...))))))) 
    ))
