;;;
;;; Justin Ethier
;;; husk scheme
;;;
;;; Sample program for file I/O.
;;;
;;; Opens a sample file containing one number per line, and outputs their sum.
;;;
;;; Usage: run from the main husk directory -
;;;        ./huski examples/simple-file-io.scm
;;;
(load "stdlib.scm")

(define *input-file* "examples/simple-file-io.txt")
(define (sum-from-file acc fp)
  (let ((data (read fp)))
      (if (eof-object? data)
        (begin
          (close-input-port fp)
          (write acc))
        (sum-from-file (+ acc data) fp))))

(sum-from-file 0 (open-input-file *input-file*))
