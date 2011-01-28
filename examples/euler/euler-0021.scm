;;
;; Justin Ethier
;;
;; TODO: this was written in chicken scheme; port it to skim-scheme
;;
;; Solution to project euler #21
;;
;; Programming Language: Scheme
;;
(define (divisor num cur divs)
  (if (equal? 0 cur) 
    divs
    (if (equal? 0 (modulo num cur))
      (divisor num (- cur 1) (append divs (list cur)))
      (divisor num (- cur 1) divs))))

(define (pdiv num)
 (divisor num (- num 1) '()))

(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))


(define (build-pdiv max h)
 (hash-table-set! h max (sum (pdiv max)))
 (if (equal? 1 max) 
     h
     (build-pdiv (- max 1) h))
)


(define (reduce-sums h count result)
 (if (equal? count 0)
  result
  (if (and (equal? count 
              (hash-table-ref/default h (hash-table-ref h count) #f))
       (not (equal? count (hash-table-ref h count))))
   (reduce-sums h (- count 1) (append result (list count)))
   (reduce-sums h (- count 1) result) 
  )
 ))

(define sums (build-pdiv 10000 (make-hash-table)))

(print "Solution: ")
(print (sum (reduce-sums sums 10000 '())))
