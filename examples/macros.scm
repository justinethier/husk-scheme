;(let ((x 1)) ;; - with this form, csi transforms (define-syntax) into (letrec-syntax), since
;             ;;   the define form is only allowed at the top-level.

(define-syntax test (syntax-rules () ((test ...) (print 1))
                                     ((test var ...) (print var ...))  
                      ))
;)
(test 1)
(test 2)
(test 1 2)

(define-syntax test-when (syntax-rules () ((when tst stmt1 stmt2 ...)
                                           (if tst (begin stmt1
                                                          stmt2 ...)))))

(test-when #t 1 2 3)
(test-when #f 1)

(define-syntax test2-when (syntax-rules () ((when tst stmt1 ...)
                                           (if tst (begin stmt1
                                                          ...)))))
; TODO: need more macros here to test with...

