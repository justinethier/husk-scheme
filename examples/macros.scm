;(let ((x 1)) ;; - with this form, csi transforms (define-syntax) into (letrec-syntax), since
;             ;;   the define form is only allowed at the top-level.
(define-syntax test (syntax-rules () ((print ...) (print 1))
                                     ((print ... ...) (print 2))  
                      ))
;)
(test 1)
(test 2)
(test 1 2)

; TODO: need more macros here to test with...

