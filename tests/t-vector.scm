;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for built-in vector forms
;;
(unit-test-start "vectors")

(assert/equal (vector? '#(1 2 3 4 5)) #t)
(assert/equal (vector? '(1 2 3 4 5)) #f)
(assert/equal (make-vector 4 "test") '#("test" "test" "test" "test"))
(assert/equal (vector-length (make-vector 4 "test")) 4)
(assert/equal (vector-length '#()) 0)
(assert/equal (vector-ref '#(1) 0) 1)

(define vec '#(1 2 3 4))
(vector-fill! vec "Num")
(assert/equal (id vec) '#("Num" "Num" "Num" "Num"))

(assert/equal (eqv? '#("Num" "Num" "Num") '#("Num" "Num" "Num")) #t)
(assert/equal (eqv? '#("Num" "Num" "Num") '#("Num" "Num" "Num2")) #f)
(assert/equal (eqv? '#("Num" "Num" "Num") '#("Num" "Num" 2)) #f)
(assert/equal (eqv? '#("1" "2" "3") '#(1 2 3)) #f)
(assert/equal (equal? '#("1" "2" "3") '#(1 2 3)) #t)

(define foo 1)
(define bar 2)
(assert/equal `#(,foo ,bar) '#(1 2))
(assert/equal `#() '#())
(assert/equal (vector 'a 'b 'c)
              '#(a b c))
(assert/equal
  (let ((vec (vector 0 '(2 2 2 2) "Anna")))
    (vector-set! vec 1 '("Sue" "Sue"))
      vec)
  '#(0 ("Sue" "Sue") "Anna"))

(assert/equal (let ((vec (vector 0 1 2 3 4)))
    (vector-fill! vec 5))
    '#(5 5 5 5 5))

(assert/equal (let ((vec (vector 0 1 2 3 4)))
    (vector->list vec))
    '(0 1 2 3 4))

(assert/equal (let ((lst (list 0 1 2 3 4)))
    (list->vector lst))
    #(0 1 2 3 4))

(assert/equal
    (vector-map cadr '#((a b) (d e) (g h)))
    #(b e h))

(assert/equal
    (vector-map (lambda (n) (expt n n))
        '#(1 2 3 4 5))
    #(1.0 4.0 27.0 256.0 3125.0))

(assert/equal
    (vector-map + '#(1 2 3) '#(4 5 6 7))
    #(5 7 9))

(assert/equal
  (let ((count 0))
    (vector-map
      (lambda (ignored)
        (set! count (+ count 1))
        count)
      '#(a b)))
    ; #(1 2)
    ; or
    ; #(2 1)
  #(2 1))
      
(assert/equal
    (let ((v (make-vector 5)))
        (vector-for-each
            (lambda (i) (vector-set! v i (* i i)))
            '#(0 1 2 3 4))
        v)
    #(0 1 4 9 16))

(assert/equal
    (vector-append #(a b c) #(d e f))
    #(a b c d e f))

(unit-test-handler-results)
