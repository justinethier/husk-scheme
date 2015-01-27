;; Temporary test file, delete before merging back to master!!
;; TODO list:
;;
;; - Compare running this in husk to chicken, there are calls missing (EG: if)
;; - Fix compiler warnings
;; - Improve call history formatting, maybe use a common function between Core/Types?
;; - Compare speed of this branch against master, can anything be sped up?
;; - What else before merging back?
(define a '(1 2 3))
(define (loop i)
  (if (= i 10)
    (read-char a)
    (loop (+ i 1))))
(loop 0)
