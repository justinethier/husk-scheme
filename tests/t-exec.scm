;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for execution-related forms 
;;
(unit-test-start "control features")

(assert/equal (procedure? car) #t "car is a procedure")
(assert/equal (procedure? 'car) #f "symbol is not a proc")
(assert/equal (procedure? (lambda (x) (* x x))) #t "lambda is a proc")
(assert/equal (procedure? '(lambda (x) (* x x)))  #f "lambda sym not a proc")
(assert/equal (call-with-current-continuation procedure?) 
              #t
              "call/cc is a proc")

(assert/equal
  (let ((v (make-vector 5)))
    (for-each (lambda (i)
                (vector-set! v i (* i i)))
              '(0 1 2 3 4))
     v)
  '#(0 1 4 9 16)
  "vector results")

(assert/equal (apply + (list 3 4)) 7 "apply results") 

(define compose
    (lambda (f g)
          (lambda args
                  (f (apply g args)))))
(assert/equal ((compose sqrt *) 12 75) 30.0 "compose")

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
 '(connect talk1 disconnect connect talk2 disconnect)
 "chat")

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
 '(connect talk1 disconnect talk2)
 "dynamic-wind not invoked") ; Continuation not invoked during dynamic extent, so before/after are not invoked

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
 '(connect talk1 disconnect talk2 talk1 disconnect)
 "dynamic wind after called twice" ) ; Because continuation captured during before, thunk/after are called twice

(define data '())
(dynamic-wind
  (lambda () (set! data (list "Invoked outer (before)")))
  (lambda () (set! data (append data (let ((path '())
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
        (reverse path)))))))
  (lambda () (set! data (append data "Invoked outer (after)"))))

(assert/equal 
    data 
    '("Invoked outer (before)" connect talk1 disconnect connect talk2 disconnect . "Invoked outer (after)")
    "dynamic-wind before and after")

(unit-test-handler-results)
