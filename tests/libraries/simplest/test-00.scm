;;;
;;; Simplest possible example, include a library
;;; with no sub-imports
;;;
(define x 'main-program)

(import (libs list))

(write  
    (list2 1 2 3))
;not-exported ; Should throw an error due to env restrictions

(write x)
