(write "test")
;Some initial test cases to work through macro expansion

(write
(let ((x 2) (y 3))
    (let ((x 7)
                  (z (+ x y)))
          (* z x)))
) ; Expected: 35

(define-syntax orr 
  (syntax-rules () 
   ((orelse <expr1> <expr2>) 
    (let ((temp <expr1>)) 
         (if temp temp <expr2>)))))

; TODO: this is broken currently
; see Issue #30
;(assert/equal
(write    (let ((temp 4)) (orr #f temp))
)
;    4)
;=> #f instead of 4

#| TODO:
; Assert that a template can be quoted, allowing someone debugging a macro the
; ability to see the expansion of that macro
(define-syntax orr-debugging 
  (syntax-rules () 
   ((orelse <expr1> <expr2>) 
    '(let ((temp <expr1>)) 
         (if temp temp <expr2>)))))

(write (orr-debugging 1 1))
; Expected:              '(let ((temp 1)) (if temp temp 1)))

(define-syntax my-do/1
  (syntax-rules ()
     ((_ ((var init . step) ...)
         (test expr ...) 
          command ...)
     (let loop ((var init) ...)
       (if test
         (begin expr ...)
         (begin (begin command ...)
                (quote (((((((loop 
                      (list var . step))))))) ...))))))))
(write
                (my-do/1 ((vec (make-vector 5) vec)
                     (i 0 (+ i 1)))
                    ((= i 5) vec)
                     (vector-set! vec i i))
)
;expected: (quote (((((((loop (list vec vec))))))) ((((((loop (list i (+ i 1)))))))))))
|#

(write (let ((x 'test)) x))
(define => 'test)

; I think this might be an example of case 2; => is defined as #f and thus that pattern should
; not be matched in (cond) ... again, I *think* that is what is going on here
;(define => 'test)
(define-syntax my-cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

((lambda () 
(write
  (let ((=> #f)) (cond (#t => 'ok)))
)))
;              'ok)

(my-cond (#t #f 'ok))
