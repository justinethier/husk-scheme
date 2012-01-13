#|
1
2
(write 3)
(write (+ 1 2 (+ 3 4 5)))
(write (+ 1 2 (+ 3 (+ 4 6) 5)))
;; TODO: returns wrong result in huskc
(write (+ 1 (+ 2) (+) 3 (- 9 2) (/ 9 3)))
(write (+ 1 (+ 2) 3 (- 9 2) (/ 9 3)))
(write (+ (+ 2) 3 (- 9 2) (/ 9 3)))
(write (+ 3 (- 9 2) (/ 9 3)))
(write (+ (- 9 2) (/ 9 3)))
(write (+ (/ 9 3))) ; results are OK
|#
(write (if 1 (+ 2 1) (+ 3 1)))
(write (if 1 (+ 2 1 4) 3))
(write (if 1 2 3))
(write (if 1 (+ 2 1 4) 3))
(write (if (+ 1) 2 3))
(write (if (+ 1) 22 3))

