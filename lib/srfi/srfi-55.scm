;;;
;;; husk-scheme
;;; http://justinethier.github.com/husk-scheme
;;;
;;; Reference implementation for SRFI-55
;;; from http://srfi.schemers.org/srfi-55/srfi-55.html
;;;
;;; Requirements: SRFI-23 (error reporting)
;;;

; 
; Example of registering extensions:
;    (register-extension '(srfi 1) "srfi/srfi-1.scm")
; Example of loading an extension:
;   (require-extension (srfi 1))
;   (require-extension (srfi 1 3 4))
;

(define *__env__* (current-environment))
(define available-extensions '())

(define (register-extension id action . compare)
  (set! available-extensions
    (cons (list (if (pair? compare) (car compare) equal?)
		id 
		action)
	  available-extensions)) )

(define (find-extension id)
  (define (lookup exts)
    (if (null? exts)
	  (write (list "extension not found - please contact your vendor " id))
	  (let ((ext (car exts)))
	    (if ((car ext) (cadr ext) id)
	        (caddr ext) ; Return a string instead of calling a function ((caddr ext))
	        (lookup (cdr exts)) ) ) ) )
  (lookup available-extensions) )

(define-syntax require-extension 
  (syntax-rules (srfi)
    ((_ (srfi id ...))
     (begin 
       (load (find-extension '(srfi id)) *__env__*) ...))))
;    ((_ "internal" id)
;     (find-extension 'id) )
;    ((_ clause ...)
;     (begin (require-extension "internal" clause) ...)) ) )

