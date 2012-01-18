(define fail
  (lambda () 999999))

(define in-range
  (lambda (a b)
    (call/cc
     (lambda (cont)
       (enumerate a b cont)))))

(define enumerate
  (lambda (a b cont)
    (if (< b a)
        (fail)
        (let ((save fail))
          (begin
            (set! fail
              (lambda ()
                (begin
                  (set! fail save)
                  (enumerate (+ a 1) b cont))))
            (cont a))))))

(let ((x (in-range 2 9))
      (y (in-range 2 9))
      (z (in-range 2 9)))
  (if (= (* x x)
         (+ (* y y) (* z z)))
      (+ (* x 100) (+ (* y 10) z))
      (fail)))
