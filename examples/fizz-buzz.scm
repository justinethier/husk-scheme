;;;
;;; Justin Ethier
;;; husk scheme
;;;
;;; A solution to FizzBuzz using named let:
;;; <http://www.codinghorror.com/blog/2007/02/why-cant-programmers-program.html>
;;;
(let fizz-buzz ((i 1))
    (if (>= 100 i)
        (begin
            (cond
                ((= 0 (modulo i 15)) (write 'fizzbuzz))
                ((= 0 (modulo i 3)) (write 'fizz))
                ((= 0 (modulo i 5)) (write 'buzz))
                (else (write i)))
            (fizz-buzz (+ i 1)))))
