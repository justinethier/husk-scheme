; This file just exists for module debugging purposes; it should
; be removed once that is up-and-running
(load "modules.scm")
;(environment '(hello world))
;(resolve-import '(hello world))
(repl-import (hello world))
;(hello (list 1 2 3))
;;; (write (expand
;;; (define-library (hello world)
;;;     (export hello)
;;;     (import (scheme base))
;;;     (begin2 (define hello "hello, world"))) ))
;;; 
;;; (let ((tmp *this-module*)) 
;;; (define (rewrite-export x) (if (pair? x) (if (if (= 3 (length x)) (eq? (quote rename) (identifier->symbol (car x))) #f) (cons (car (cddr x)) (cadr x)) (error "invalid module export" x)) x))
;;; (define (extract-exports) (write "entered extract-exports") (write *this-module*) ((lambda (temp4145) (if temp4145 ((lambda (x46) (if (pair? (cdr x46)) (error "export-all takes no parameters" x46)) #f) temp4145) (if #t ((lambda () ((lambda (f5158) ((lambda (ff5362) ((lambda () (ff5362 *this-module* (quote ()))))) ((lambda (proc5465) (f5158 (lambda (ls66 res67) ((proc5465 proc5465) ls66 res67)))) (lambda (proc5468) (f5158 (lambda (ls69 res70) ((proc5468 proc5468) ls69 res70))))))) (lambda (lp71) (lambda (ls72 res73) (if (null? ls72) ((lambda () res73)) (if (if (pair? (car ls72)) (eq? (quote export) (caar ls72)) #f) ((lambda () (lp71 (cdr ls72) (append (map rewrite-export (cdar ls72)) res73)))) (if #t ((lambda () (lp71 (cdr ls72) res73)))))))))))))) (assq (quote export-all) *this-module*)))
;;; (set! *this-module* (quote ()))
;;; (set! *this-module* (cons (quote (export hello)) *this-module*))
;;; (set! *this-module* (cons (quote (import (scheme base))) *this-module*))
;;; (set! *this-module* (cons (quote (begin2 (define hello "hello, world"))) *this-module*))
;;; (set! *this-module* (reverse *this-module*))
;;; (add-module!37 (quote (hello world)) (make-module (extract-exports) #f *this-module*))
;;; (set! *this-module* tmp))
