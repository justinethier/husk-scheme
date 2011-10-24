(define-syntax my-let
  (syntax-rules ()
    ((_ ((x v) ...) e1 e2 ...)
     ((lambda (x ...) e1 e2 ...) v ...))))


; TODO: the problem below is that the mx in (+ mx my) is renamed to the mx in
;       the inner my-let, instead of the outer mx
;
; ; I think there is also a hygiene problem when the 'm' is removed from var names
(write 
  (my-let ((mx 2) (my 3)) (my-let ((mx 7) (mz (+ mx my))) (* mz mx))))

; test cases for hygienic macro rework

; TODO: idea, can we rename a var at the beginning of transformRule (atom)?
; I think it is wrong to call renameVars to rename in a list, as per below:

; Following test case does not work, output below:
;
;a = let t = (let ((x 7)) x1) ex = {(let (((x1 y2) 7)) x1), True, 1} expanded = ((lambda () x1) 7)
;
;some initial thoughts:
;
;perhaps each recursively expanded macro needs its own environment, to prevent the outer x from
;clobbering x in the inner macro.
;(write (let ((x 2) (y 3)) (let ((x 7)) x)))

;(write (let ((z 1)) z (+ z z)))
;(write (let ((z 1)) (+ z z)))


;(write
;  (let ((z 1)) ((lambda () z))))
;
;(write
;  (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))
;
;(write (let ((x 2) (y 3)) (+ x y)))
;
;
;(write (let ((name 'a)) `(list ,name . ,name)))
;(write (let ((name 'a)) `(list 1 . 1)))
;
; TODO: need to handle this case (currently it truncates the second 'name'
;(write (let ((name 'a)) `(list name . name))) ; Special post-processing req'd??
