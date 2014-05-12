;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for special forms 
;;
(unit-test-start "special forms")

(assert/equal (procedure? car) #t)
(assert/equal (procedure? 1) #f)
(assert/equal (procedure? "procedure") #f)

(assert/equal (procedure? load) #t)
(assert/equal (procedure? apply) #t)
(assert/equal (procedure? eval) #t)
(assert/equal (procedure? call-with-current-continuation) #t)

;(assert/equal (string '())) "")
(assert/equal (list->string '(#\a)) "a")
(assert/equal (list->string '(#\s #\k #\i #\m)) "skim")
(assert/equal (list->string '()) "")

(assert/equal (vector->string #(#\a #\b #\c) 2 3) "c")
(assert/equal (string->vector "abc" 2 3) #(#\c))

(define a "12345")
(define b "abcde")
(string-copy! b 1 a 0 2)
(assert/equal b "a12de")
(string-copy! a 1 b 2 4)
(assert/equal a "12de5")

(assert/equal
    (let ((ls (list 'one 'two 'five!)))
        (list-set! ls 2 'three)
        ls)
    '(one two three))

(unit-test-handler-results)
