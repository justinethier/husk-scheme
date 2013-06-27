;;;
;;; Justin Ethier
;;; husk scheme
;;;
;;; A sample library
;;;
(define-library (libs list)
    (export list2)
    (begin
        (define (list2 . objs)  objs)
        (define x 'libs-list)
        (define not-exported 'should-not-be-exported)
        ))
