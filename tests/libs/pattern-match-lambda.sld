(define-library (libs pattern-match-lambda)
  ;(export pattern-match-lambda)
  (export-all) ; pattern-match-lambda)
  (import (scheme base))
(begin

(define-syntax if-identifier
  (syntax-rules ()
    ((_ condition seq alt)
     (let-syntax ((foo (syntax-rules () ((_) seq))))
       (let-syntax ((test (syntax-rules ()
                            ((_ condition) (foo))
                            ((_ foo) alt))))
         (test foo))))))

(define-syntax if-literal
  (syntax-rules ()
    ((_ p (literals ...) seq alt)
     (let-syntax ((bar (syntax-rules () ((_) seq))))
       (let-syntax ((foo (syntax-rules (literals ...)
                         ((_ literals) (bar)) ...
                         ((_ bar) alt))))
         (foo p))))))

(define-syntax if-placeholder
  (syntax-rules (_)  ;; Literals cannot include underbar in R6RS.
    ((_ _ seq alt) seq)
    ((_ p seq alt) alt)))

(define-syntax %if-match-vector
  (syntax-rules ()
    ((_ (literals ...) #() ind e seq alt) seq)
    ((_ (literals ...) #(p r ...) ind e seq alt)
     (%if-match (literals ...) p (vector-ref e ind)
       (let ((i ind))
         (%if-match-vector (literals ...) #(r ...) (+ i 1) e seq alt))
       alt))))

(define-syntax %if-match
  (syntax-rules ()
    ((_ (literals ...) #(p ...) e seq alt)
     (if (and (vector? e) (= (vector-length '#(p ...)) (vector-length e)))
         (%if-match-vector (literals ...) #(p ...) 0 e seq alt)
         (alt)))
    ((_ (literals ...) (p . r) e seq alt)
     (let ((temp e))
       (if (pair? temp)
           (%if-match (literals ...) p (car temp)
             (%if-match (literals ...) r (cdr temp) seq alt)
             alt)
           (alt))))
    ((_ (literals ...) () e seq alt)
     (if (null? e) seq (alt)))
    ((_ (literals ...) p e seq alt)
     (if-identifier p
       (if-literal p (literals ...)
         (if (equal? 'p e) seq (alt))
         (if-placeholder p
           seq
           (let ((p e)) seq)))
       (if (equal? p e) seq (alt))))))

(define-syntax %duplicate-check
  (syntax-rules ()
    ((_) #f)
    ((_ p r ...)
     (letrec-syntax
         ((bar (syntax-rules ()
                 ((_) (syntax-error "duplicate pattern variable in pattern-match-lambda" p ))))
          (foo (syntax-rules (r ...)
                 ((_ r) (bar))
                 ...
                 ((_ x) (%duplicate-check r ...)))))
                 ;JAE - broken in husk (non-standard?): ((_ x) (syntax-rules () ((_) (%duplicate-check r ...)))))))
       (foo p)))))

(define-syntax duplicate-check
  (syntax-rules ()
    ((_ (pvar ...) (literals ...) #(p ...))
     (duplicate-check (pvar ...) (literals ...) (p ...)))
    ((_ (pvar ...) (literals ...) ((p) . r))
     (duplicate-check (pvar ...) (literals ...) (p . r)))
    ((_ (pvar ...) (literals ...) ((p0 . p1) . r))
     (duplicate-check (pvar ...) (literals ...) (p0 p1 . r)))
    ((_ (pvar ...) (literals ...) (#(p ...) . r))
     (duplicate-check (pvar ...) (literals ...) (p ... . r)))
    ((_ (pvar ...) (literals ...) (p . r))
     (if-identifier p
       (if-literal p (literals ...)
         (duplicate-check (pvar ...) (literals ...) r)
         (if-placeholder p
           (duplicate-check (pvar ...) (literals ...) r)
           (duplicate-check (pvar ... p) (literals ...) r)))
       (duplicate-check (pvar ...) (literals ...) r)))
    ((_ (pvar ...) (literals ...) ())
     (%duplicate-check pvar ...))
    ((_ (pvar ...) (literals ...) p)
     (if-identifier p
       (if-literal p (literals ...)
         (duplicate-check (pvar ...) (literals ...) ())
         (if-placeholder p
           (duplicate-check (pvar ...) (literals ...) ())
           (duplicate-check (pvar ... p) (literals ...) ())))
       (duplicate-check (pvar ...) (literals ...) ())))))

(define-syntax if-match
  (syntax-rules ()
    ((_ (literals ...) pattern lst seq alt)
     (let ((alt-thunk (lambda() alt)))
       (begin (duplicate-check () (literals ...) pattern)
              (%if-match (literals ...) pattern lst seq alt-thunk))))))

(define-syntax %pattern-match-lambda
  (syntax-rules (else)
    ((_ (literals ...) lst) (if #f #t))
    ((_ (literals ...) lst (else expr))
     expr)
    ((_ (literals ...) lst (pattern expr) (rest-pattern rest-expr) ...)
     (if-match (literals ...) pattern lst
       expr
       (%pattern-match-lambda (literals ...) lst
         (rest-pattern rest-expr) ...)))
    ((_ (literals ...) lst (pattern fender expr) (rest-pattern rest-expr) ...)
     (let ((next
            (lambda() (%pattern-match-lambda (literals ...) lst
                                             (rest-pattern rest-expr) ...))))
     (if-match (literals ...) pattern lst
       (if fender expr (next))
       (next))))))

(define-syntax pattern-match-lambda
  (syntax-rules ()
    ((_ (literals ...) clause  ...)
     (lambda lst
       (%pattern-match-lambda (literals ...) lst clause ...)))))

))
