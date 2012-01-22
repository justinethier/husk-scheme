1
(write (((lambda () +)) 1 2 3 4))
(write (if #f #t #f))
(write (if ((lambda () #t)) (+ 100) (+ 200)))
(write (if ((lambda () #f)) (+ 100) (+ 200)))
1
2
;(write (+ 11 22 33))
(write (+ 1 2 3))
(write (+ (+ 1) (+ (+ 2 2)) (+ (+ 3 3) (+ (+ 3)))))
4
1
2
(write (+ 11 22 33))
3
#|
1
2
(write 3)
2
(write (+ 1 2 (+ 3 4 5)))
(write (+ 1 2 (+ 3 (+ 4 6) 5)))
(write (+ 1 (+ 2) (+) 3 (- 9 2) (/ 9 3)))
(write (+ 1 (+ 2) 3 (- 9 2) (/ 9 3)))
(write (+ (+ 2) 3 (- 9 2) (/ 9 3)))
(write (+ 3 (- 9 2) (/ 9 3)))
(write (+ (- 9 2) (/ 9 3)))
(write (+ (/ 9 3))) ; results are OK
(write (if (+ 1 (+ 2) (+) 3 (- 9 2) (/ 9 3)) 
           (+ 1 (+ 2) (+) 3 (- 9 2) (/ 9 3))
           (+ 1 (+ 2) (+) 3 (- 9 2) (/ 9 3))))
(write (if 1 (+ 2 1) (+ 3 1)))
(write (if 1 (+ 2 1 4) 3))
(write (if 1 2 3))
(write (if 1 (+ 2 1 4) 3))
(write (if (+ 1) 2 3))
(write (if #f 2 #f))
(write (if (+ 1) 22 3))

(write ((lambda () 1 2 (+ 3) )))
(write ((lambda () 1 2 3 )))
((lambda () 1 2 (+ 3 4)))
; TODO: (write (if #t (+ ((lambda () 1 2 3) 3)) 4))
|#
