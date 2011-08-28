
(write "TODO: the following are not transformed correctly by husk:")

; A simple example of a pair in the pattern and transform
(define-syntax pair-test
  (syntax-rules ()
     ((_ (a b . c))
      (quote (a b . c)))))
(write (pair-test (1 2 . 3)))
;(1 2 . 3)
(write (pair-test (1 2 3)))
;(1 2 3)
(write (pair-test (1 2 3 . 4)))
;(1 2 3 . 4)
(write (pair-test (1 2)))
;(1 2)
(write (pair-test (1 2 3 4 5 6)))
;(1 2 3 4 5 6)

(define-syntax pair-test-2
  (syntax-rules ()
     ((_ ((a) (b) . (c)))
      (quote ((a) (b) . (c))))))
(write (pair-test ((1) (2) . (3))))
(write (pair-test ((1) (2) (3))))
(write (pair-test ((1) (2) (3) . (4))))
(write (pair-test ((1) (2))))
(write (pair-test ((1) (2) (3) (4) (5) (6))))

(define-syntax pair-test-nested
  (syntax-rules ()
     ((_ (a b . c) ...)
      (quote (a b . c) ...))))
(write (pair-test-nested (1 2 . 3)))
;(1 2 . 3)
(write (pair-test-nested (1 2 3)))
;(1 2 3)
(write (pair-test-nested (1 2 3 . 4)))
;(1 2 3 . 4)
(write (pair-test-nested (1 2)))
;(1 2)
(write (pair-test-nested (1 2 3 4 5 6)))
;(1 2 3 4 5 6)

#|
; Would be surprized if this worked in husk at the moment...
;(write (pair-test (1 2 . )))
;;(1 2)

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
|#
