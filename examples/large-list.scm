(require-extension (srfi 1))

(define x (iota 10000))
(define (f idx) 
    (if (= idx 0) 
        x 
        (begin 
            x 
            (car x)
            (f (- idx 1)))))

