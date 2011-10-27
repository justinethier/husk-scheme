;Some initial test cases to work through macro expansion

(write
(let ((x 2) (y 3))
    (let ((x 7)
                  (z (+ x y)))
          (* z x)))
) ; Expected: 35
