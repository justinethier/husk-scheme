;;;
;;; Justin Ethier
;;; husk scheme
;;;
;;; Sample program for file I/O
;;; still needs work
;;;
;;; TODO: do something interesting like summing up all integers in a file
;;;
(define a (open-input-file "examples/simple-file-io.txt"))
(read a)
(read a) ; TODO: need to return EOF to program instead of crashing...

;See (eof-object? obj) in the spec - a new object of this type must be returned when EOF is detected

(close-input-port a)
