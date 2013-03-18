;;;
;;; Justin Ethier
;;; husk scheme
;;;
;;; A sample program demonstrating how to use libraries.
;;; To run, go to the directory containing this file and
;;; execute it using huski:
;;;
;;; > huski hello.scm
;;;
(import (scheme r5rs base)
        (rename (prefix (libs lib1) test-)))

(test-lib1-hello)
