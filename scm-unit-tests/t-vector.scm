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

(unit-test-handler-results)
