;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for I/O-related forms 
;;
(unit-test-start "I/O")

(define *filename* "_Test.txt")
(define *list-data* '(1 2 ((3)) (4 5 6) ((((7))))))
(define ofp (open-output-file *filename*))

(assert/equal (output-port? ofp) #t)
(assert/equal (input-port? ofp) #f)
(assert/equal (output-port? ofp) #t)
(assert/equal (input-port? ofp) #f)
(write *list-data* ofp)
(close-output-port ofp)
(assert/equal (file-exists? *filename*) #t)

(define ifp (open-input-file *filename*))
(assert/equal (output-port? ifp) #f)
(assert/equal (input-port? ifp) #t)
(assert/equal (char-ready? ifp) #t)
(assert/equal (peek-char ifp) #\()

(let ((data (read ifp)))
  (assert/equal data *list-data*))

(close-input-port ifp)
(assert/equal (file-exists? *filename*) #t)
(assert/equal (delete-file *filename*) #t)
(assert/equal (delete-file *filename*) #f)
(assert/equal (file-exists? *filename*) #f)

(unit-test-handler-results)
