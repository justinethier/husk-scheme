;; Temporary test file, delete before merging back to master!!
;; TODO list:
;;
;; - Compare running this in husk to chicken, there are calls missing (EG: if)
;; - Compare speed of this branch against master, can anything be sped up?
;; - Before merging back: check docs, eliminate compiler warnings
;; - Compiler does not produce any call history
((lambda ()
    (list? (list))
    (define a '(1 2 3))
    (define (loop i)
      (if (read-char a) ;(= i 10)
        (read-char a)
        (loop (+ i 1))))
    (loop 0)))
