(define a "12345")
(define b (string-copy "abcde"))
(string-copy! b 0 a) ; 0 2)
(write a)
(write b)

(define a (vector 1 2 3 4 5))
(define b (vector 10 20 30 40 50))
(vector-copy! b 0 a)
(write a)
(write b)
;(vector-copy! b 1 a 0 2)

