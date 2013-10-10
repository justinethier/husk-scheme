(define (string-map fnc . sargs)
    (let* ((ls (map (lambda (s) (string->list s)) sargs)))
        (list->string 
            (apply map 
                   (cons fnc ls)))))
(define (string-for-each fnc . sargs)
    (let ((ls (map (lambda (v) (string->list v)) sargs)))
        (apply for-each 
               (cons fnc ls))))
(write
(string-map
    (lambda (c)
        (integer->char (+ 1 (char->integer c))))
        "HAL")
)
;"IBM"

(write
    (let ((v '()))
        (string-for-each
            (lambda (c) (set! v (cons (char->integer c) v)))
            "abcde")
        v)
)
;(101 100 99 98 97)
