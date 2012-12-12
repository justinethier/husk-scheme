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
;


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
;
; TODO: rewrite below using (load filename env) to account for the begin
;
; A temporary version that works for one srfi, but needs to be
; generalized. The issue is that introducing a new scope will
; cause the load to be ineffective because the outer scope will
; not be affected
    ((_ (srfi id ...))
     (load (find-extension '(srfi id) ...)) ) ; TODO: Maybe we could have a load-all that accepts a list?
    ))
;    ((_ "internal" (srfi id ...))
;     (begin (find-extension '(srfi id) ...)) )
;    ((_ "internal" id)
;     (find-extension 'id) )
;    ((_ clause ...)
;     (begin (require-extension "internal" clause) ...)) ) )

