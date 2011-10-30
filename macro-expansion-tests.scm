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
