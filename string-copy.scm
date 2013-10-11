(define-syntax def-copy-in-place
    (er-macro-transformer
        (lambda (expr rename compare)
            (let* ((base (symbol->string (cadr expr)))
                   (sym (lambda (rstr)
                            (string->symbol
                                 (string-append base "-" rstr)))))
            `(define (,(sym "copy!") to at from)
                (do ((i 0 (+ i 1)))
                    ((= i (,(sym "length") from)) to)
                     (,(sym "set!") to (+ at i) (,(sym "ref") from i))))))))

; TODO: this is just a temporary work area, integrate back into husk
(def-copy-in-place string)
(define a "12345")
(define b (string-copy "abcde"))
(string-copy! b 0 a) ; 0 2)
(write a)
(write b)

(def-copy-in-place vector)
(define a (vector 1 2 3 4 5))
(define b (vector 10 20 30 40 50))
(vector-copy! b 0 a)
(write a)
(write b)
;(vector-copy! b 1 a 0 2)

