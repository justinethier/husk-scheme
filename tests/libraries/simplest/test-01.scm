;;;
;;; This test case demonstrates including two top-level libraries
;;;
(define x 'main-program)

(import (libs list)
        (libs lib1))

(write  
    (list2 1 2 3))
;not-exported ; Should throw an error due to env restrictions

(write x)
(write lib1-hello)
