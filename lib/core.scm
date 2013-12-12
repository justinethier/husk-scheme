;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; Standard library of scheme functions
;;;

(define call/cc call-with-current-continuation)

(define (not x)      (if x #f #t))

(define (list . objs)  objs)
(define (id obj)       obj)

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

(define (sum . lst)     (foldl + 0 lst))
(define (product . lst) (foldl * 1 lst))
(define (square z) (* z z))

(define (exact-integer-sqrt x)
  (let ((res (sqrt x)))
    (if (exact? res)
        (values res 0)
        (let ((res (inexact->exact (truncate res))))
          (values res (- x (* res res)))))))

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

(define (max first . rest) (foldl (lambda (old new) (if (> old new) old new)) first rest))
(define (min first . rest) (foldl (lambda (old new) (if (< old new) old new)) first rest))

(define zero?        (curry = 0))
(define positive?    (curry < 0))
(define negative?    (curry > 0))
(define (odd? num)   (= (modulo num 2) 1))
(define (even? num)  (= (modulo num 2) 0))
(define (exact-integer? num)
    (and (exact? num) (integer? num)))

(define (length lst)    (foldl (lambda (x y) (+ y 1)) 0 lst))
(define (reverse lst)   (foldl cons '() lst))

(define-syntax begin
  (syntax-rules ()
    ((begin exp ...)
      ((lambda () exp ...)))))

(define-syntax include
  (syntax-rules ()
    ((include file1 file2 ...)
     (begin
       (load file1) 
       (load file2) ...))))

;
;
; NOTE: The below cond/case forms do NOT use begin to prevent
;       conflicts between the stdlib begin and the begin form
;       from the module metalanguage.
;
; TODO: this may indicate a problem with syntax-rules and
;       referential transparency.
;
;

; cond
; Form from R5RS:
(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
;
; TODO: see pitfall 3.2
;
; This is a modification from R5RS - we put the begin within
; an if statement, because in this context definitions are
; not allowed. This prevents one from interfering with macro
; hygiene. 
;
; TODO: unfortunately the macro logic has not yet been
; updated to take this into acccount, so the pitfall
; still fails
;
     ((lambda () result1 result2 ...))) ;; Intentionally not using begin, see above
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
     (if test ((lambda () result1 result2 ...)))) ;; Intentionally not using begin, see above
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         ((lambda () result1 result2 ...)) ;; Intentionally not using begin, see above
         (cond clause1 clause2 ...)))))
; Case
; Form from R5RS:
(define-syntax case
  (syntax-rules (else =>)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else => result))
     (result key))
    ((case key
       (else result1 result2 ...))
     (if #t ((lambda () result1 result2 ...)))) ;; Intentionally not using begin, see above
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         ((lambda () result1 result2 ...)))) ;; Intentionally not using begin, see above
    ((case key
       ((atoms ...) => result)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (result key)
         (case key clause clauses ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         ((lambda () result1 result2 ...)) ;; Intentionally not using begin, see above
         (case key clause clauses ...)))))

(define (my-mem-helper obj lst cmp-proc)
 (cond 
   ((null? lst) #f)
   ((cmp-proc obj (car lst)) lst)
   (else (my-mem-helper obj (cdr lst) cmp-proc))))
(define (memq obj lst) (my-mem-helper obj lst eq?))
(define (memv obj lst) (my-mem-helper obj lst eqv?))
(define (member obj lst . compare) 
    (if (pair? compare)
        (my-mem-helper obj lst (car compare))
        (my-mem-helper obj lst equal?)))

(define (mem-helper pred op)  (lambda (next acc) (if (and (not acc) (pred (op next))) next acc)))
(define (assq obj alist)      (foldl (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist)      (foldl (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist . compare)
    (if (pair? compare)
        (foldl (mem-helper (curry (car compare) obj) car) #f alist)
        (foldl (mem-helper (curry equal? obj) car) #f alist)))

; SRFI 8
; Reference implementation from: http://srfi.schemers.org/srfi-8/srfi-8.html
;
; FUTURE: This may be moved into its own file
;
(define-syntax receive
    (syntax-rules ()
        ((receive formals expression body ...)
         (call-with-values (lambda () expression)
             (lambda formals body ...)))))
; END SRFI 8

; Added the following support functions from SRFI 1
(define (car+cdr pair) (values (car pair) (cdr pair)))
(define (%cars+cdrs lists)
  (call-with-current-continuation
    (lambda (abort)
      (let recur ((lists lists))
        (if (pair? lists)
	    (receive (list other-lists) (car+cdr lists)
	      (if (null? list) (abort '() '()) ; LIST is empty -- bail out
		  (receive (a d) (car+cdr list)
		    (receive (cars cdrs) (recur other-lists)
		      (values (cons a cars) (cons d cdrs))))))
	    (values '() '()))))))
; END support functions

(define (map f lis1 . lists)
;  (check-arg procedure? f map-in-order)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
        (receive (cars cdrs) (%cars+cdrs lists)
          (if (pair? cars)
              (let ((x (apply f cars)))		; Do head first,
                (cons x (recur cdrs)))		; then tail.
              '())))
      ;; Fast path.
     (foldr (lambda (x y) (cons (f x) y)) '() lis1)))

(define (for-each f lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
        (receive (cars cdrs) (%cars+cdrs lists)
          (if (pair? cars)
              (begin
                (apply f cars)
                (recur cdrs)))))
      ;; Fast path.
      (if (eq? 1 (length lis1))
        (f (car lis1))
        (begin (f (car lis1))
               (for-each f (cdr lis1))))))

(define (list-tail lst k) 
        (if (zero? k)
          lst
          (list-tail (cdr lst) (- k 1))))
(define (list-ref lst k)  (car (list-tail lst k)))

; Let forms
;
; letrec from R5RS
(define-syntax letrec
    (syntax-rules ()
      ((letrec ((var1 init1) ...) body ...)
       (letrec "generate_temp_names"
         (var1 ...)
         ()
         ((var1 init1) ...)
         body ...))
      ((letrec "generate_temp_names"
         ()
         (temp1 ...)
         ((var1 init1) ...)
         body ...)                         ; start the changed code
       (let ((var1 #f) ...)
         (let ((temp1 init1) ...)
           (set! var1 temp1)
           ...
           body ...)))
      ((letrec "generate_temp_names"
         (x y ...)
         (temp ...)
         ((var1 init1) ...)
         body ...)
       (letrec "generate_temp_names"
         (y ...)
         (newtemp temp ...)
         ((var1 init1) ...)
         body ...))))

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
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...)
       body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
         body1 body2 ...)))))

; Iteration - do
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
     y)))

; End delayed evaluation section

; String Section
(define-syntax string-fill!
  (syntax-rules ()
    ((_ _str _chr)
     (set! _str
           (make-string (string-length _str) _chr)))))

(define (string-concatenate l) 
    (apply string-append l)) 

; Vector Section
(define-syntax vector-fill!
  (syntax-rules ()
    ((_ _vec _fill)
     (set! _vec
           (make-vector (vector-length _vec) _fill)))))

; Bytevector Section

; TODO: add appropriate overloads, also note this is not the
; fastest implementation
(define (bytevector-copy! to at from start end)
  (do ((i 0 (+ i 1)))
      ((= i (- end start)))
    (bytevector-u8-set! 
        to
        (+ at i) 
        (bytevector-u8-ref from (+ start i)))))

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

;; SRFI 23 - Error reporting mechanism
;; based on code from: http://srfi.schemers.org/srfi-23/srfi-23.html
(define (error reason . args)
    (display "Error: ")
    (display reason)
    (newline)
    (for-each (lambda (arg) 
                (display " ")
      	  (write arg))
      	args)
    (newline)
    (exit-fail))

;; Hashtable derived forms
(define hash-table-walk
  (lambda (ht proc)
    (map 
      (lambda (kv) (proc (car kv) (car (reverse kv))))
      (hash-table->alist ht)))) 

(define (hash-table-update! hash-table key function)
  (hash-table-set! hash-table key
                  (function (hash-table-ref hash-table key))))

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

; Implementations of gcd and lcm using Euclid's algorithm
;
; Also note that each form is written to accept either 0 or
; 2 arguments, per R5RS. This could probably be generalized
; even further, if necessary.
;
(define gcd '())
(define lcm '())

(let ()
  ; Main GCD algorithm
  (define (gcd/main a b)
    (if (= b 0)
      (abs a)
      (gcd/main b (modulo a b))))

  ; A helper function to reduce the input list
  (define (gcd/entry . nums)
    (if (eqv? nums '())
      0
      (foldl gcd/main (car nums) (cdr nums))))

  ; Main LCM algorithm
  (define (lcm/main a b)
    (abs (/ (* a b) (gcd/main a b))))

  ; A helper function to reduce the input list
  (define (lcm/entry . nums)
    (if (eqv? nums '())
      1
      (foldl lcm/main (car nums) (cdr nums))))

  (set! gcd gcd/entry)
  (set! lcm lcm/entry))  

; append accepts a variable number of arguments, per R5RS. So a wrapper
; has been provided for the standard 2-argument version of (append).
;
; We return the given value if less than 2 arguments are given, and
; otherwise fold over each arg, appending it to its predecessor. 
(define (append . lst)
  (define append-2
          (lambda (inlist alist) 
                  (foldr (lambda (ap in) (cons ap in)) alist inlist)))
  (if (null? lst)
      lst
      (if (null? (cdr lst))
          (car lst)
          (foldl (lambda (a b) (append-2 b a)) (car lst) (cdr lst)))))

; Quasi-quotation as a macro
; Based on code from chibi-scheme
;
; The code below is compiled to avoid having to expand dozens of macros
; in real-time, significantly improving performance. This does highlight
; an area that husk could improve upon but for now a compilation will do.
;
; The (expand) special form is used to compile the code, in case it needs
; to be changed in the future.
;
(define-syntax quasiquote
  (er-macro-transformer
   (lambda (expr rename compare)
(define (qq x d) (if (pair? x) ((lambda () (if (compare (rename (quote unquote)) (car x)) ((lambda () (if (<= d 0) (cadr x) (list (rename (quote list)) (list (rename (quote quote)) (quote unquote)) (qq (cadr x) (- d 1)))))) (if (compare (rename (quote unquote-splicing)) (car x)) ((lambda () (if (<= d 0) (list (rename (quote cons)) (qq (car x) d) (qq (cdr x) d)) (list (rename (quote list)) (list (rename (quote quote)) (quote unquote-splicing)) (qq (cadr x) (- d 1)))))) (if (compare (rename (quote quasiquote)) (car x)) ((lambda () (list (rename (quote list)) (list (rename (quote quote)) (quote quasiquote)) (qq (cadr x) (+ d 1))))) (if (if (<= d 0) (if (pair? (car x)) (compare (rename (quote unquote-splicing)) (caar x)) #f) #f) ((lambda () (if (null? (cdr x)) (cadr (car x)) (list (rename (quote append)) (cadr (car x)) (qq (cdr x) d))))) (if #t ((lambda () (list (rename (quote cons)) (qq (car x) d) (qq (cdr x) d))))))))))) (if (vector? x) ((lambda () (list (rename (quote list->vector)) (qq (vector->list x) d)))) (if (if (symbol? x) #t (null? x)) ((lambda () (list (rename (quote quote)) x))) (if #t ((lambda () x)))))))
     (qq (cadr expr) 0))))
;; Original code:
;     (define (qq x d)
;       (cond
;        ((pair? x)
;         (cond
;          ((compare (rename 'unquote) (car x))
;           (if (<= d 0)
;               (cadr x)
;               (list (rename 'list) (list (rename 'quote) 'unquote)
;                     (qq (cadr x) (- d 1)))))
;          ((compare (rename 'unquote-splicing) (car x))
;           (if (<= d 0)
;               (list (rename 'cons) (qq (car x) d) (qq (cdr x) d))
;               (list (rename 'list) (list (rename 'quote) 'unquote-splicing)
;                     (qq (cadr x) (- d 1)))))
;          ((compare (rename 'quasiquote) (car x))
;           (list (rename 'list) (list (rename 'quote) 'quasiquote)
;                 (qq (cadr x) (+ d 1))))
;          ((and (<= d 0) (pair? (car x))
;                (compare (rename 'unquote-splicing) (caar x)))
;           (if (null? (cdr x))
;               (cadr (car x))
;               (list (rename 'append) (cadr (car x)) (qq (cdr x) d))))
;          (else
;           (list (rename 'cons) (qq (car x) d) (qq (cdr x) d)))))
;        ((vector? x) (list (rename 'list->vector) (qq (vector->list x) d)))
;        ((if (symbol? x) #t (null? x)) (list (rename 'quote) x))
;        (else x)))
;     (qq (cadr expr) 0))))


;;
;; Vector functions from r7rs
;; These help round-out the standard list/vector/string functions
(define (vector-map fnc . vargs)
    (let* ((ls (map (lambda (v) (vector->list v)) vargs)))
        (list->vector 
            (apply map 
                   (cons fnc ls)))))

(define (vector-for-each fnc . vargs)
    (let ((ls (map (lambda (v) (vector->list v)) vargs)))
        (apply for-each 
               (cons fnc ls))))

(define (vector-append . vargs)
    (let ((ls (map (lambda (v) (vector->list v)) vargs)))
        (list->vector 
            (apply append 
                   ls))))
(define (vector->string v)
    (list->string 
        (vector->list v)))

(define (string->vector s)
    (list->vector
        (string->list s)))
;;
;; String functions from r7rs
(define (string-map fnc . sargs)
    (let* ((ls (map (lambda (s) (string->list s)) sargs)))
        (list->string 
            (apply map 
                   (cons fnc ls)))))
(define (string-for-each fnc . sargs)
    (let ((ls (map (lambda (v) (string->list v)) sargs)))
        (apply for-each 
               (cons fnc ls))))
;; END

;; More functions from r7rs
(define (features) *features*)
(define-syntax def-copy-in-place
    (er-macro-transformer
        (lambda (expr rename compare)
            (let* ((base (symbol->string (cadr expr)))
                   (sym (lambda (rstr)
                            (string->symbol
                                 (string-append base "-" rstr)))))
            `(define (,(sym "copy!") to at from)
                (do ((i 0 (+ i 1)))
                    ((= i (,(sym "length") from)) to)
                     (,(sym "set!") to (+ at i) (,(sym "ref") from i))))))))
(def-copy-in-place string)
(def-copy-in-place vector)
;; END

;; Simplified versions of every/any from SRFI-1
(define (any pred lst)
  (let any* ((l (map pred lst)))
      (cond
        ((null? l) #f) ; Empty list
        ((car l)   #t) ; Done
        (else 
           (any* (cdr l))))))
(define (every pred lst)
  (let every* ((l (map pred lst)))
      (cond
        ((null? l) #t) ; Empty list
        ((car l)
           (every* (cdr l)))
        (else 
           #f))))

;; Macros from r7rs
(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test
         (begin result1 result2 ...)))))
(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test)
         (begin result1 result2 ...)))))
(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((var1 init1) ...) body1 body2 ...)
     (let ((var1 #f) ...)
        (set! var1 init1)
        ...
        (let () body1 body2 ...)))))
(define-syntax syntax-error
  (er-macro-transformer
   (lambda (expr rename compare)
     (apply error (cdr expr)))))

(define-syntax
  let-values
  (syntax-rules
    ()
    ((let-values (binding ...) body0 body1 ...)
     (let-values
       "bind"
       (binding ...)
       ()
       ((lambda () body0 body1 ...))))
       ;(begin body0 body1 ...)))
    ((let-values "bind" () tmps body)
     (let tmps body))
    ((let-values
       "bind"
       ((b0 e0) binding ...)
       tmps
       body)
     (let-values
       "mktmp"
       b0
       e0
       ()
       (binding ...)
       tmps
       body))
    ((let-values
       "mktmp"
       ()
       e0
       args
       bindings
       tmps
       body)
     (call-with-values
       (lambda () e0)
       (lambda args
         (let-values "bind" bindings tmps body))))
    ((let-values
       "mktmp"
       (a . b)
       e0
       (arg ...)
       bindings
       (tmp ...)
       body)
     (let-values
       "mktmp"
       b
       e0
       (arg ... x)
       bindings
       (tmp ... (a x))
       body))
    ((let-values
       "mktmp"
       a
       e0
       (arg ...)
       bindings
       (tmp ...)
       body)
     (call-with-values
       (lambda () e0)
       ;(lambda (arg ... x)
       (lambda (arg ... . x)
         (let-values "bind" bindings (tmp ... (a x)) body))))))

;;
;; SRFI-0 (cond-expand) from r7rs
;;
(define-syntax cond-expand
  (er-macro-transformer
   (lambda (expr rename compare)
     (define (identifier->symbol i) i)
     (define (check x)
       (if (pair? x)
           (case (car x)
             ((and) (every check (cdr x)))
             ((or) (any check (cdr x)))
             ((not) (not (check (cadr x))))
             ((library) (eval `(module-exists? ',(cadr x)) *meta-env*))
             (else (error "cond-expand: bad feature" x)))
           (memq (identifier->symbol x) *features*)))
     (let expand ((ls (cdr expr)))
       (cond ((null? ls))  ; (error "cond-expand: no expansions" expr)
             ((not (pair? (car ls))) (error "cond-expand: bad clause" (car ls)))
             ((eq? 'else (identifier->symbol (caar ls)))
              (if (pair? (cdr ls))
                  (error "cond-expand: else in non-final position")
                  `(,(rename 'begin) ,@(cdar ls))))
             ((check (caar ls)) `(,(rename 'begin) ,@(cdar ls)))
             (else (expand (cdr ls))))))))
;; END
