; TODO: string-copy! and vector-copy! are structurally identical and could both be generated using the same macro
(define (string-copy! to at from)
    (do ((i 0 (+ i 1)))
        ((= i (string-length from)) to)
         (string-set! to (+ at i) (string-ref from i))))
(define a "12345")
(define b (string-copy "abcde"))
(string-copy! b 0 a) ; 0 2)
(write a)
(write b)

(define (vector-copy! to at from)
    (do ((i 0 (+ i 1)))
        ((= i (vector-length from)) to)
         (vector-set! to (+ at i) (vector-ref from i))))

(define a (vector 1 2 3 4 5))
(define b (vector 10 20 30 40 50))
(vector-copy! b 0 a)
(write a)
(write b)
;(vector-copy! b 1 a 0 2)

