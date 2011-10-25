;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for the refactored macro module 
;;
(load "skim-unit.scm")
(unit-test-start "macros (refactored)")

(define-syntax my-let
  (syntax-rules ()
    ((_ ((x v) ...) e1 e2 ...)
     ((lambda (x ...) e1 e2 ...) v ...))))


; TODO: the problem below is that the mx in (+ mx my) is renamed to the mx in
;       the inner my-let, instead of the outer mx
;
; More thoughts:
;
; ***I need to think about this more and convince myself*** , but I think the answer to get
; this to work is to implement renaming (Clinger's algorithm) for macro calls as well as lambda.
; Since they both work together, I do not think it will be possible to only implement lambda's and
; not run into problems such as this.
;
; Perhaps this should be phased, and we should try to get everything working with the lambda
; renaming disabled. Then once everything is up and running, then both lambda and macro calls
; will have to be implemented together
;
; At least macro def's can wait until after the other two are implemented...
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


(assert/equal (let ((x 2) (y 3)) (let ((x 7)) x)) 7)

(assert/equal (let ((z 1)) z (+ z z)) 2)
(assert/equal (let ((z 1)) (+ z z)) 2)

(assert/equal
  (let ((z 1)) ((lambda () z)))
  1)


; TODO: these 2 cause the intepreter to hang:
;(write
;  (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))

;(write (let ((x 2) (y 3)) (+ x y)))
;
;
(assert/equal (let ((name 'a)) `(list ,name . ,name))
             '(list a . a))
(assert/equal (let ((name 'a)) `(list 1 . 1))
             '(list 1 . 1))
;
; TODO: need to handle this case (currently it truncates the second 'name'
;(write (let ((name 'a)) `(list name . name))) ; Special post-processing req'd??

(unit-test-handler-results)
