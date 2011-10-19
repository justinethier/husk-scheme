; test cases for hygienic macro rework

;(write
;  (let ((z 1)) ((lambda () z))))
;
;(write
;  (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))
;
(write
  (let ((name 'a)) `(list ,name . ,name)))
