; test cases for hygienic macro rework

(write (let ((x 2) (y 3)) (let ((x 7)) x)))

(write
  (let ((z 1)) ((lambda () z))))

(write
  (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))

(write (let ((x 2) (y 3)) (+ x y)))


(write (let ((name 'a)) `(list ,name . ,name)))
(write (let ((name 'a)) `(list 1 . 1)))

; TODO: need to handle this case (currently it truncates the second 'name'
;(write (let ((name 'a)) `(list name . name))) ; Special post-processing req'd??
