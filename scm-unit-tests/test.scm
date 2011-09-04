(load "../stdlib.scm")

;; TODO: the problem here is that these cases have an improper list in the pattern and a proper list in the transform
;; the passing cases had improper lists in both.
;;
;; The only issue here is that we are currently appending '() to the lists in the pattern. this is going to be 
;; problematic, may need to consider using an external data structure to store a boolean flag, rather than 
;; appending the data to the pattern/input data
(write "TODO: the following are not transformed correctly by husk:")

; Would be surprized if this worked in husk at the moment...
;(write (pair-test (1 2 . )))
;;(1 2)

#| TODO: output from csi:
(1 2 3)
(1 2 (3))
(1 2 (3 4))
(1 2 ())
(1 2 (3 4 5 . 6))
((1 2 3))
((1 2 (3)))
((1 2 ()))
|#

; A temporary test to demonstrate what is happening in the other tests below.
; We do not handle the case where a variable is in an nary match in the pattern but is
; used directly in the transform. This should certainly be an error in some cases such
; as this one. But the pair/list ones below are real an absolutely must be handled.
(define-syntax test
  (syntax-rules ()
     ((_ (a b c ...))
      (quote (a b c)))))
(write (test (1 2)))

; A macro taking a pair in the pattern and transforming into a list
(define-syntax pair-test-00
  (syntax-rules ()
     ((_ (a b . c))
      (quote (a b c)))))
(write (pair-test-00 (1 2 . 3)))
;(1 2 3)

(write (pair-test-00 (1 2 3)))
;(1 2 (3))

(write (pair-test-00 (1 2 3 4)))
;(1 2 (3 4))
(write (pair-test-00 (1 2)))
;(1 2 ())
(write (pair-test-00 (1 2 3 4 5 . 6)))
(write (pair-test-00 (1 2 3 . 6)))
;(1 2 (3 4 5 . 6))

; Same as previous macro, but packaged using (list) - any real difference here?
(define-syntax pair-test-01
  (syntax-rules ()
     ((_ (a b . c))
      (list (quote (a b c))))))

(write (pair-test-01 (1 2 . 3)))
; ((1 2 3))

(write (pair-test-01 (1 2 3)))
; ((1 2 (3)))

(write (pair-test-01 (1 2)))
;((1 2 ()))
;
; TODO: test with this macro:
;
;(define-syntax pair-test-00
;  (syntax-rules ()
;     ((_ (a b . ((c))))
;      (quote (a b c)))))
