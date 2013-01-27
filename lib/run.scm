; This file just exists for module debugging purposes; it should
; be removed once that is up-and-running
(load "modules.scm")
(resolve-import '(hello world))

; ;(write (expand
; ;(define-library (hello world)
; ;    (export hello)
; ;    (import (scheme base))
; ;    (begin (define hello "hello, world"))) ))
; ;(define *this-module*35 *this-module*)
; (let ((tmp *this-module*35)) (define (rewrite-export x) (if (pair? x) (if (if (= 3 (length x)) (eq? (quote rename) (identifier->symbol (car x))) #f) (cons (car (cddr x)) (cadr x)) (error "invalid module export" x)) x)) (define (extract-exports) (write "entered extract-exports") (write *this-module*35) ((lambda (temp4044) (if temp4044 ((lambda (x45) (if (pair? (cdr x45)) (error "export-all takes no parameters" x45)) #f) temp4044) (if #t ((lambda () ((lambda (f5057) ((lambda (ff5261) ((lambda () (ff5261 *this-module*35 (quote ()))))) ((lambda (proc5364) (f5057 (lambda (ls65 res66) ((proc5364 proc5364) ls65 res66)))) (lambda (proc5367) (f5057 (lambda (ls68 res69) ((proc5367 proc5367) ls68 res69))))))) (lambda (lp70) (lambda (ls71 res72) (if (null? ls71) ((lambda () res72)) (if (if (pair? (car ls71)) (eq? (quote export) (caar ls71)) #f) ((lambda () (lp70 (cdr ls71) (append (map rewrite-export (cdar ls71)) res72)))) (if #t ((lambda () (lp70 (cdr ls71) res72)))))))))))))) (assq (quote export-all) *this-module*35))) 
; 
; (set! *this-module*35 (quote ()))
; (set! *this-module*89 
;     (cons (quote (export hello)) *this-module*89)) 
; (set! *this-module*93 (cons (quote (import (scheme base))) *this-module*93))
; ((lambda () 
;     (define hello "hello, world"))) 
; (set! *this-module*35 (reverse *this-module*35)) 
; (add-module!36 (quote (hello world)) (make-module (extract-exports) #f *this-module*35)) 
; (set! *this-module*35 tmp))

I think the issue above is that the form (set! *tm* (cons _ *tm)) is forming a circular dep or at least not preserving the previous version of *tm*

Or, could be that all of the renamed variables are supposed to refer to the same memory location, but they do not... so this disconnect prevents the code from working properly
