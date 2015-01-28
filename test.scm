;; Temporary test file, delete before merging back to master!!
;; TODO list:
;;
;; - Uh oh, running tail-call-opt example shows unbound memory growth!!
;; - Compare speed of this branch against master, can anything be sped up?
;;   maybe try running the profiler some more too, esp for computationally-intensive code
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
