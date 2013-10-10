(define (string-map fnc . sargs)
    (let* ((ls (map (lambda (s) (string->list s)) sargs)))
        (list->string 
            (apply map 
                   (cons fnc ls)))))
(write
(string-map
    (lambda (c)
        (integer->char (+ 1 (char->integer c))))
        "HAL")
)
;"IBM"
