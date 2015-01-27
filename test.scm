;; Temporary test file, delete before merging back to master!!
(define a '(1 2 3))
(define (loop i)
  (if (= i 10)
    (read-char a)
    (loop (+ i 1))))
(loop 0)
