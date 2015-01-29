;; Temporary test file, delete before merging back to master!!
;; TODO list:
;;
;; - Compiler does not produce any call history
((lambda ()
    (list? (list))
    (define a '(1 2 3))
    (define (loop i)
      ;(if (= i 10)
      (if (read-char a) ;(= i 10)
        (read-char a)
        (loop (+ i 1))))
    (loop 0)))
