;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for execution-related forms 
;;
(load "skim-unit.scm")
(unit-test-start "control features")

(assert/equal (procedure? car) #t)
(assert/equal (procedure? 'car) #f)
(assert/equal (procedure? (lambda (x) (* x x))) #t)
(assert/equal (procedure? '(lambda (x) (* x x)))  #f)
(assert/equal (call-with-current-continuation procedure?) #t)

(assert/equal
  (let ((v (make-vector 5)))
    (for-each (lambda (i)
                (vector-set! v i (* i i)))
              '(0 1 2 3 4))
     v)
  #(0 1 4 9 16))

(assert/equal (apply + (list 3 4)) 7) 

(define compose
    (lambda (f g)
          (lambda args
                  (f (apply g args)))))
(assert/equal ((compose sqrt *) 12 75) 30.0)

(assert/equal
 (let ((path '())
      (c #f))
  (let ((add (lambda (s)
               (set! path (cons s path)))))
    (dynamic-wind
      (lambda () (add 'connect))
      (lambda ()
        (add (call-with-current-continuation
               (lambda (c0)
                 (set! c c0)
                 'talk1))))
      (lambda () (add 'disconnect)))
    (if (< (length path) 4)
        (c 'talk2)
        (reverse path))))
 '(connect talk1 disconnect connect talk2 disconnect))

(assert/equal
  (let ((path '())
      (c #f))
  (let ((add (lambda (s)
               (set! path (cons s path)))))
    (dynamic-wind
      (lambda () (add 'connect))
      (lambda () (add 'talk1))
      (lambda ()
        (add (call-with-current-continuation
               (lambda (c0)
                 (set! c c0)
                 'disconnect)))))
    (if (< (length path) 4)
        (c 'talk2)
        (reverse path))))
 '(connect talk1 disconnect talk2)) ; Continuation not invoked during dynamic extent, so before/after are not invoked

(assert/equal
  (let ((path '())
      (c #f))
  (let ((add (lambda (s)
               (set! path (cons s path)))))
    (dynamic-wind
      (lambda ()
        (add (call-with-current-continuation
               (lambda (c0)
                 (set! c c0)
                 'connect))))
      (lambda () (add 'talk1))
      (lambda () (add 'disconnect)))
    (if (< (length path) 4)
        (c 'talk2)
        (reverse path))))
 '(connect talk1 disconnect talk2 talk1 disconnect)) ; Because continuation captured during before, thunk/after are called twice

(unit-test-handler-results)
