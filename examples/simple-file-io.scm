;;;
;;; Justin Ethier
;;; husk scheme
;;;
;;; Sample program for file I/O
;;; still needs work
;;;
(define a (open-input-file "examples/simple-file-io.txt"))
(read a)
(read a)
