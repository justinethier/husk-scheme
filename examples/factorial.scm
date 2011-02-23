;;; 
;;; Justin Ethier
;;; husk-scheme
;;;
;;; Simple program to calculate the factorial of a number
;;;
(define factorial
  (lambda (n)
    (if (= n 0) 1
        (* n (factorial (- n 1))))))
(factorial 25000)
