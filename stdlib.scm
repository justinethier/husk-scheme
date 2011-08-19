;
; husk-scheme
; http://github.com/justinethier/husk-scheme
;
; Written by Justin Ethier
;
; Standard library of scheme functions
;
(define call/cc call-with-current-continuation)

(define (caar pair) (car (car pair)))
(define (cadr pair) (car (cdr pair)))
(define (cdar pair) (cdr (car pair)))
(define (cddr pair) (cdr (cdr pair)))
(define (caaar pair) (car (car (car pair))))
(define (caadr pair) (car (car (cdr pair))))
(define (cadar pair) (car (cdr (car pair))))
(define (caddr pair) (car (cdr (cdr pair))))
(define (cdaar pair) (cdr (car (car pair))))
(define (cdadr pair) (cdr (car (cdr pair))))
(define (cddar pair) (cdr (cdr (car pair))))
(define (cdddr pair) (cdr (cdr (cdr pair))))
(define (caaaar pair) (car (car (car (car pair)))))
(define (caaadr pair) (car (car (car (cdr pair)))))
(define (caadar pair) (car (car (cdr (car pair)))))
(define (caaddr pair) (car (car (cdr (cdr pair)))))
(define (cadaar pair) (car (cdr (car (car pair)))))
(define (cadadr pair) (car (cdr (car (cdr pair)))))
(define (caddar pair) (car (cdr (cdr (car pair)))))
(define (cadddr pair) (car (cdr (cdr (cdr pair)))))
(define (cdaaar pair) (cdr (car (car (car pair)))))
(define (cdaadr pair) (cdr (car (car (cdr pair)))))
(define (cdadar pair) (cdr (car (cdr (car pair)))))
(define (cdaddr pair) (cdr (car (cdr (cdr pair)))))
(define (cddaar pair) (cdr (cdr (car (car pair)))))
(define (cddadr pair) (cdr (cdr (car (cdr pair)))))
(define (cdddar pair) (cdr (cdr (cdr (car pair)))))
(define (cddddr pair) (cdr (cdr (cdr (cdr pair)))))


(define (not x)      (if x #f #t))

(define (list . objs)  objs)
(define (id obj)       obj)

(define (flip func)    (lambda (arg1 arg2) (func arg1 arg2)))

(define (curry func arg1)  (lambda (arg) (apply func (cons arg1 (list arg)))))
(define (compose f g)      (lambda (arg) (f (apply g arg))))

(define (foldr func end lst)
  (if (null? lst)
	  end
	  (func (car lst) (foldr func end (cdr lst)))))

(define (foldl func accum lst)
  (if (null? lst)
	  accum
	  (foldl func (func (car lst) accum) (cdr lst))))

(define fold foldl)
(define reduce fold)

(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))

(define (sum . lst)     (fold + 0 lst))
(define (product . lst) (fold * 1 lst))

; Forms from R5RS for and/or
(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

(define (abs num)
  (if (negative? num)
      (* num -1)
      num))

(define (max first . rest) (fold (lambda (old new) (if (> old new) old new)) first rest))
(define (min first . rest) (fold (lambda (old new) (if (< old new) old new)) first rest))

(define zero?        (curry = 0))
(define positive?    (curry < 0))
(define negative?    (curry > 0))
(define (odd? num)   (= (modulo num 2) 1))
(define (even? num)  (= (modulo num 2) 0))

(define (length lst)    (fold (lambda (x y) (+ y 1)) 0 lst))
(define (reverse lst)   (fold (flip cons) '() lst))

; cond
; Form from R5RS:
(define-syntax cond
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
; Case
;
; TODO: form from R5RS:
;(define-syntax case
;  (syntax-rules (else)
;    ((case (key ...)
;       clauses ...)
;     (let ((atom-key (key ...)))
;       (case atom-key clauses ...)))
;    ((case key
;       (else result1 result2 ...))
;     (begin result1 result2 ...))
;    ((case key
;       ((atoms ...) result1 result2 ...))
;     (if (memv key '(atoms ...))
;         (begin result1 result2 ...)))
;    ((case key
;       ((atoms ...) result1 result2 ...)
;       clause clauses ...)
;     (if (memv key '(atoms ...))
;         (begin result1 result2 ...)
;         (case key clause clauses ...)))))
;
; Based loosely on implementation from:
; http://blog.jcoglan.com/2009/02/25/announcing-heist-a-new-scheme-implementation-written-in-ruby/
(define-syntax case
  (syntax-rules (else)
    ((_ key) ((lambda () #f)))
    ((_ key (else expr1 expr2 ...))
     (begin expr1 expr2 ...))
    ((_ key (() expr ...) clause ...)
     (case key clause ...))
    ((_ key
           ((datum1 datum2 ...) expr1 expr2 ...)
           clause ...)
     (if (eqv? #f (memv key '(datum1 datum2 ...)))
          (case key clause ...)
          (begin expr1 expr2 ...)))))

(define (my-mem-helper obj lst cmp-proc)
 (cond 
   ((null? lst) #f)
   ((cmp-proc obj (car lst)) lst)
   (else (my-mem-helper obj (cdr lst) cmp-proc))))
(define (memq obj lst) (my-mem-helper obj lst eq?))
(define (memv obj lst) (my-mem-helper obj lst eqv?))
(define (member obj lst) (my-mem-helper obj lst equal?))

(define (mem-helper pred op)  (lambda (next acc) (if (and (not acc) (pred (op next))) next acc)))
(define (assq obj alist)      (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist)      (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist)     (fold (mem-helper (curry equal? obj) car) #f alist))

; FUTURE: on map and for-each - Support variable number of args, per spec:
; http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.4
;(define (for-each func . lsts) )

(define (for-each func lst) 
  (if (eq? 1 (length lst))
	(func (car lst))
    (begin (func (car lst))
           (for-each func (cdr lst)))))

(define (map func lst)        (foldr (lambda (x y) (cons (func x) y)) '() lst))
(define (filter pred lst)     (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))


(define (list-tail lst k) 
        (if (zero? k)
          lst
          (list-tail (cdr lst) (- k 1))))
(define (list-ref lst k)  (car (list-tail lst k)))

(define (append inlist alist) (foldr (lambda (ap in) (cons ap in)) alist inlist))

; Let forms
(define-syntax letrec
  (syntax-rules ()
    ((_ ((x v) ...) e1 e2 ...)
     (let () 
       (define x v) ...
       (let () e1 e2 ...)))))

; let and named let (using the Y-combinator):
(define-syntax let
  (syntax-rules ()
    ((_ ((x v) ...) e1 e2 ...)
     ((lambda (x ...) e1 e2 ...) v ...))
    ((_ name ((x v) ...) e1 e2 ...)
     (let*
       ((f  (lambda (name)
              (lambda (x ...) e1 e2 ...)))
        (ff ((lambda (proc) (f (lambda (x ...) ((proc proc)
               x ...))))
             (lambda (proc) (f (lambda (x ...) ((proc proc)
               x ...)))))))
        (ff v ...)))))

;
; It would be nice to change first rule back to:
;    ((_ () body) body)
;
(define-syntax let*
  (syntax-rules ()
    ((_ () body) ((lambda () body)))
    ((_ ((var val)
		 (vars vals) ...)
		 body)
	 (let ((var val))
       (let* ((vars vals) ...)
		     body)))))


; Iteration - do
(define-syntax do
  (syntax-rules ()
     ((_ ((var init . step) ...)
         (test expr ...) 
          command ...)
     (let loop ((var init) ...)
       (if test
         (begin expr ...)
         (begin (begin command ...)
                (loop 
                  (if (null? (cdr (list var . step))) 
                      (car  (list var . step))
                      (cadr (list var . step))) ...)))))))
#| TODO: this is the macro from R5RS
(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
         command ...)
     (letrec
       ((loop
         (lambda (var ...)
           (if test
               (begin
                 (if #f #f)
                 expr ...)
               (begin
                 command
                 ...
                 (loop (do "step" var step ...)
                       ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
|#
; Delayed evaluation functions
(define force
    (lambda (object)
	      (object)))

(define-syntax delay 
  (syntax-rules () 
    ((delay expression)
     (make-promise (lambda () expression)))))

(define make-promise
  (lambda (proc)
    (let ((result-ready? #f)
          (result #f))
      (lambda ()
        (if result-ready? 
            result
            (let ((x (proc)))
              (if result-ready?
                  result
                  (begin (set! result x)
                         (set! result-ready? #t)
                         result))))))))

; End delayed evaluation section

; String Section
(define-syntax string-fill!
  (syntax-rules ()
    ((_ _str _chr)
     (set! _str
           (make-string (string-length _str) _chr)))))

; Vector Section
(define-syntax vector-fill!
  (syntax-rules ()
    ((_ _vec _fill)
     (set! _vec
           (make-vector (vector-length _vec) _fill)))))

; Continuation Section
(define (values . things)
    (call-with-current-continuation 
        (lambda (cont) (apply cont things))))

;; I/O Section
(define (newline . port)
  (if (null? port) 
      (display #\newline) 
      (display #\newline port)))

; TODO: test these forms
(define (call-with-input-file filename proc)
  (let ((opened-file (open-input-file filename)))
    (define result
           (proc opened-file))
    (close-input-port opened-file)
    result))
; TODO: test
(define (call-with-output-file filename proc)
  (let ((opened-file (open-output-file filename)))
    (define result
           (proc opened-file))
    (close-output-port opened-file)
    result))

;; Hashtable derived forms
(define hash-table-walk
  (lambda (ht proc)
    (map 
      (lambda (kv) (proc (car kv) (car (reverse kv))))
      (hash-table->alist ht)))) 

(define (hash-table-update! hash-table key function)
  (hash-table-set! hash-table key
                  (function (hash-table-ref hash-table key thunk))))

(define-syntax hash-table-merge!
  (syntax-rules ()
    ((_ hdest hsrc)
     (map (lambda (node) (hash-table-set! hdest 
                                       (car node)
                                       (cadr node)))
       (hash-table->alist hsrc)))))

(define (alist->hash-table lst)
 (let ((ht (make-hash-table)))
   (for-each (lambda (node)
              (hash-table-set! ht (car node) (cadr node)))
             lst)
   ht))

(define (hash-table-fold hash-table f acc-in)
  (let ((acc acc-in))
    (hash-table-walk hash-table 
             (lambda (key value) (set! acc (f key value acc))))
      acc))


; FUTURE: Issue #10: from numeric section -
; gcd
;(define (gcd . nums) 
;  (if (eqv? nums '())
;    0
;    TBD...))

; Limited form of gcd from: http://www.dreamincode.net/code/snippet1358.htm
(define (gcd a b)
    (if (= b 0) a
            (gcd b (modulo a b))))
; lcm
; rationalize
; 

