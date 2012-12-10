;;;
;;; Justin Ethier
;;; husk scheme
;;;
;;; Sample program for file I/O.
;;;
;;; Opens a sample file containing one number per line, and outputs their sum
;;; both to the screen and to an output file.
;;;
;;; Usage: huski simple-file-io.scm
;;;

(define *input-file* "simple-file-io.txt")
(define *output-file* "simple-file-io.out")

(define (sum-from-file acc fp)
  (let ((data (read fp)))
      (if (eof-object? data)
        (begin
          (write (input-port? fp))
          (write (output-port? fp))
          (close-input-port fp)
          (write acc)
          acc)
        (sum-from-file (+ acc data) fp))))

(define (write-result result fp)
  (write (input-port? fp))
  (write (output-port? fp))
  (write result fp)
  (close-output-port fp))

(write-result
    (sum-from-file 0 (open-input-file *input-file*))
    (open-output-file *output-file*))
