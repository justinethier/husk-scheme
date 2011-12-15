; From:
; http://okmij.org/ftp/Scheme/syntax-rule-dark-corner.scm
;
; A dark, under-specified corner of R5RS macros
;
; When you load this file into your Scheme system, you might see the
; number '3' printed. Or perhaps, number '4'. Some Scheme system may
; report an unbound variable error. The existing Scheme systems almost
; evenly fall into three categories regarding this test.
;
; See macros.html#syntax-rule-dark-corner for discussion.
;
; $Id: syntax-rule-dark-corner.scm,v 1.1 2003/03/09 23:48:16 oleg Exp oleg $

; This is almost the 'letrec' from R5RS modulo the changes noted
; below.

(define-syntax letrec1
    (syntax-rules ()
      ((letrec1 ((var1 init1) ...) body ...)
       (letrec1 "generate_temp_names"
         (var1 ...)
         ()
         ((var1 init1) ...)
         body ...))
      ((letrec1 "generate_temp_names"
         ()
         (temp1 ...)
         ((var1 init1) ...)
         body ...)                         ; start the changed code
	(begin
	  (define var1 #f) ...
	  (define temp1 init1) ...
	  (set! var1 temp1) ...
	  body ...))			   ; end the changed code
      ((letrec1 "generate_temp_names"
         (x y ...)
         (temp ...)
         ((var1 init1) ...)
         body ...)
       (letrec1 "generate_temp_names"
         (y ...)
         (newtemp temp ...)
         ((var1 init1) ...)
         body ...))))

(letrec1 ((x 1) (y 2)) (display (+ x y)) (newline))

; If we load this code into Scheme48, we see the number "3" printed.
;
; SCM with a built-in macro-expander 
; (scm -r5 -l syntax-rule-dark-corner.scm) prints 4.
; Gambit with a preloaded syntax-case.scm (downloaded from Gambit's web
; page) prints 4.
; Gauche Scheme prints 4.
; Al Petrofsky's portable expander gives 4.
;
; SCM using William Clinger's macros that work implementation:
;    scm -u -e '(load (in-vicinity (library-vicinity) "macwork"))'\
;           -e '(macro:load "syntax-rule-dark-corner.scm")'
; prints an error:
;    "unbound variable:  newtemp in (#@set! x newtemp)"
; Petite Scheme 6.0 prints
;    "Error: variable newtemp is not bound"
; Bigloo 2.4b prints
;    "Unbound variable -- newtemp"
