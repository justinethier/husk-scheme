; !!!
; TODO: DELETE THIS BEFORE MERGING BACK INTO MASTER!!
; !!!
(define (rename r) r)
(define (compare a b) (eqv? a b))

; Based on code from chibi-scheme
(define test
   (lambda (expr rename compare)
     (write "testing:")
     (write expr)
     (newline)
     (define (qq x d)
       (cond
        ((pair? x)
         (cond
          ((compare (rename 'unquote) (car x))
           (if (<= d 0)
               (cadr x)
               (list (rename 'list) (list (rename 'quote) 'unquote)
                     (qq (cadr x) (- d 1)))))
          ((compare (rename 'unquote-splicing) (car x))
           (if (<= d 0)
               (list (rename 'cons) (qq (car x) d) (qq (cdr x) d))
               (list (rename 'list) (list (rename 'quote) 'unquote-splicing)
                     (qq (cadr x) (- d 1)))))
          ((compare (rename 'quasiquote) (car x))
           (list (rename 'list) (list (rename 'quote) 'quasiquote)
                 (qq (cadr x) (+ d 1))))
          ((and (<= d 0) (pair? (car x))
                (compare (rename 'unquote-splicing) (caar x)))
           (if (null? (cdr x))
               (cadr (car x))
               (list (rename 'append) (cadr (car x)) (qq (cdr x) d))))
          (else
           (list (rename 'cons) (qq (car x) d) (qq (cdr x) d)))))
        ((vector? x) (list (rename 'list->vector) (qq (vector->list x) d)))
        ((if (symbol? x) #t (null? x)) (list (rename 'quote) x))
        (else x)))
     (qq (cadr expr) 0)))

;(write (test '`(1 2) rename compare))

(define-syntax test2
  (er-macro-transformer
   (lambda (expr rename compare)
     (test expr rename compare))))

(newline)
(write (test2 `(1 2) rename compare))
(newline)

;(let-syntax 
; ((call
;  (er-macro-transformer
;    (lambda (exp rename compare)
;      (define (x y) list)
;        (x 1
;          )))))
;    (call 1 2 3 4))
