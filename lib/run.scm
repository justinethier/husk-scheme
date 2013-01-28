; This file just exists for module debugging purposes; it should
; be removed once that is up-and-running
(load "modules.scm")
;(environment '(hello world))
;(resolve-import '(hello world))
(repl-import (hello world))

;(write (expand
;(define-library (hello world)
;    (export hello)
;    (import (scheme base))
;    (begin (define hello "hello, world"))) ))

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
