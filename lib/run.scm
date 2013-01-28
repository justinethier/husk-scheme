; This file just exists for module debugging purposes; it should
; be removed once that is up-and-running
(load "modules.scm")
;(environment '(hello world))
;(resolve-import '(hello world))

(write (expand
(define-library (hello world)
    (export hello)
    (import (scheme base))
    (begin (define hello "hello, world"))) ))

; Here is the latest output/analysis of the above. It looks like there is a problem with capturing the 'hello' definition:
;
;;; (let ((tmp *this-module*)) 
;;; 
;;; (define (rewrite-export x) (if (pair? x) (if (if (= 3 (length x)) (eq? (quote rename) (identifier->symbol (car x))) #f) (cons (car (cddr x)) (cadr x)) (error "invalid module export" x)) x))
;;; 
;;; (define (extract-exports) (write "entered extract-exports") (write *this-module*) ((lambda (temp3943) (if temp3943 ((lambda (x44) (if (pair? (cdr x44)) (error "export-all takes no parameters" x44)) #f) temp3943) (if #t ((lambda () ((lambda (f4956) ((lambda (ff5160) ((lambda () (ff5160 *this-module* (quote ()))))) ((lambda (proc5263) (f4956 (lambda (ls64 res65) ((proc5263 proc5263) ls64 res65)))) (lambda (proc5266) (f4956 (lambda (ls67 res68) ((proc5266 proc5266) ls67 res68))))))) (lambda (lp69) (lambda (ls70 res71) (if (null? ls70) ((lambda () res71)) (if (if (pair? (car ls70)) (eq? (quote export) (caar ls70)) #f) ((lambda () (lp69 (cdr ls70) (append (map rewrite-export (cdar ls70)) res71)))) (if #t ((lambda () (lp69 (cdr ls70) res71)))))))))))))) (assq (quote export-all) *this-module*)))
;;; 
;;; (set! *this-module* (quote ()))
;;; (set! *this-module* (cons (quote (export hello)) *this-module*))
;;; (set! *this-module* (cons (quote (import (scheme base))) *this-module*))
;;; ; JAE - looks like the 'hello' def will be lost. hmm...
;;; ((lambda () (define hello "hello, world")))
;;; (set! *this-module* (reverse *this-module*))
;;; (add-module!35 (quote (hello world)) (make-module (extract-exports) #f *this-module*))
;;; (set! *this-module* tmp))


; Old output:
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

;I think the issue above is that the form (set! *tm* (cons _ *tm)) is forming a circular dep or at least not preserving the previous version of *tm*
;
;Or, could be that all of the renamed variables are supposed to refer to the same memory location, but they do not... so this disconnect prevents the code from working properly
