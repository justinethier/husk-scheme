; TODO: this is a testpad, need to integrate all this back into husk
(define (vector-map fnc . vargs)
    (let* ((ls (map (lambda (v) (vector->list v)) vargs))
           (ms (apply map (cons fnc ls)))
;           (vs (map (lambda (l) (list->vector l) ms)))
           )
        (list->vector ms)
        ))

(define (vector-for-each fnc . vargs)
    (let ((ls (map 
                (lambda (v) (vector->list v)) 
                vargs)))
        (apply for-each 
               (cons fnc ls))))

(define (vector-append . vargs)
    (let ((ls (map 
                (lambda (v) (vector->list v)) 
                vargs)))
        (list->vector (apply append ls))))

(write
(vector-map cadr '#((a b) (d e) (g h)))
    ;#(b e h)
)

(write
(vector-map (lambda (n) (expt n n))
    '#(1 2 3 4 5))
    ;#(1 4 27 256 3125)
)

(write
(vector-map + '#(1 2 3) '#(4 5 6 7))
    ;#(5 7 9)
)

(let ((count 0))
    (vector-map
      (lambda (ignored)
        (set! count (+ count 1))
        count)
      '#(a b)))
    ; #(1 2)
    ; or
    ; #(2 1)
        
(write
(let ((v (make-vector 5)))
    (vector-for-each
        (lambda (i) (vector-set! v i (* i i)))
        '#(0 1 2 3 4))
    v)
)
;(0 1 4 9 16)

(write
    (vector-append #(a b c) #(d e f)))
;#(a b c d e f)
