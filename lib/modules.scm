; NOTE: This file is experimental, and not currently used by husk
;
; An attempt to modify chibi scheme's module meta language
; to use to implement modules (r7rs libraries) in husk
;

;; meta.scm -- meta langauge for describing modules
;; Copyright (c) 2009-2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modules

;; JAE - Hacks to get everything to work
(define *modules* '()) ; Just a temporary def, see EOF
; env-exports - I think this needs to be a hook in husk

; %import - needs to be a hook in husk
; The signature in chibi is:
;sexp sexp_env_import_op (sexp ctx, sexp self, sexp_sint_t n, sexp to, sexp from, sexp ls, sexp immutp) {
; and example usage is:
;(%import env (module-env mod2) (cdr mod2-name+imports) #t)))
;
; Parameters:
;  1) environment to import to (?)
;  2) env to import from
;  3) list of defs to import?
;  4) immutable flag?
;; (define (%import to from ls immutp)
;;   (write "%import test stub")
;;   (write ls)
;;   (write "end %import")
;; )
;  (write ls))
;

; PROGRESS LOG:
;
; Working on porting this to husk. The trouble is, after loading a
; module, the contents of *modules* is:
;
;  (((hello world) . #((hello) #f ((export hello)) #f)))
;
; So Where-T-F is the environment?!??
;
;
; UPDATE 1/28 - after adding begin2 and *this-module* hacks, am
; able to successfully import a binding. but the binding is not added
; to the current env - not sure why:
;
;;;;  huski> (load "run.scm" )
;;;;  ("loading file" "hello/world.sld")
;;;;  "entered extract-exports"
;;;;  ((export hello) (begin2 (define hello "hello, world")))
;;;;  Import toEnv: "hello, world"
;;;;  finished import
;;;;  <env>
;;;;  huski> hello
;;;;  Getting an unbound variable: hello
;;;;  huski> (define e (repl-import (hello world)))
;;;;  Import toEnv: "hello, world"
;;;;  finished import
;;;;  <env>
;;;;  huski> e
;;;;  <env>
;;;;  huski> hello
;;;;  Getting an unbound variable: hello
;;;;  huski> (eval 'hello)
;;;;  Getting an unbound variable: hello
;;;;  huski> (eval 'hello e)
;;;;  "hello, world"
;
;
;; /JAE


(define *this-module* '())

(define (make-module exports env meta) (vector exports env meta #f))
(define (%module-exports mod) (vector-ref mod 0))
(define (module-env mod) (vector-ref mod 1))
(define (module-env-set! mod env) (vector-set! mod 1 env))
(define (module-meta-data mod) (vector-ref mod 2))
(define (module-meta-data-set! mod x) (vector-set! mod 2 x))

(define (module-exports mod)
  (or (%module-exports mod) (env-exports (module-env mod))))
; JAE  (or (%module-exports mod) #| JAE - (env-exports (module-env mod)) |# ))

(define (module-name->strings ls res)
  (if (null? ls)
      res
      (let ((str (cond ((symbol? (car ls)) (symbol->string (car ls)))
                       ((number? (car ls)) (number->string (car ls)))
                       ((string? (car ls)) (car ls))
                       (else (error "invalid module name" (car ls))))))
        (module-name->strings (cdr ls) (cons "/" (cons str res))))))

(define (module-name->file name)
  (string-concatenate
   (reverse (cons ".sld" (cdr (module-name->strings name '()))))))

(define (module-name-prefix name)
  (string-concatenate (reverse (cdr (cdr (module-name->strings name '()))))))

; TODO: test this def below:
; WTF is find-module-file  ?
(define load-module-definition
  (let ((meta-env (current-environment)))
    (lambda (name)
      (let* ((file (module-name->file name))
; JAE - TODO: this version prepends a path
;  to the filename. husk will eventually need
;  to do this as well...
;             (path (find-module-file file)))
;        (if path (load path meta-env))))))
        )
        (write (list "loading file" file)) ; JAE debugging
        (if file (load file))))))

(define (find-module name)
  (cond
   ((assoc name *modules*) => cdr)
   (else
    (load-module-definition name)
    (cond ((assoc name *modules*) => cdr)
          (else #f)))))

(define (add-module! name module)
  (set! *modules* (cons (cons name module) *modules*)))

(define (delete-module! name)
  (let lp ((ls *modules*) (prev #f))
    (cond ((null? ls))
          ((equal? name (car (car ls)))
           (if prev
               (set-cdr! prev (cdr ls))
               (set! *modules* (cdr ls))))
          (else (lp (cdr ls) ls)))))

(define (symbol-append a b)
  (string->symbol (string-append (symbol->string a) (symbol->string b))))

(define (symbol-drop a b)
  (let ((as (symbol->string a))
        (bs (symbol->string b)))
    (if (and (> (string-length bs) (string-length as))
             (string=? as (substring bs 0 (string-length as))))
        (string->symbol (substring bs (string-length as) (string-length bs)))
        b)))

;; (define (warn msg . args)
;;   (display msg (current-error-port))
;;   (display ":" (current-error-port))
;;   (for-each (lambda (a)
;;               (display " " (current-error-port))
;;               (write a (current-error-port)))
;;             args)
;;   (newline (current-error-port)))

(define (to-id id) (if (pair? id) (car id) id))
(define (from-id id) (if (pair? id) (cdr id) id))
(define (id-filter pred ls)
  (cond ((null? ls) '())
        ((pred (to-id (car ls))) (cons (car ls) (id-filter pred (cdr ls))))
        (else (id-filter pred (cdr ls)))))

(define (resolve-import x)
  (cond
   ((not (and (pair? x) (list? x)))
    (error "invalid module syntax" x))
   ((and (memq (car x) '(prefix drop-prefix))
         (symbol? (car (cddr x))) (list? (cadr x)))
    (let ((mod-name+imports (resolve-import (cadr x))))
      (cons (car mod-name+imports)
            (map (lambda (i)
                   (cons ((if (eq? (car x) 'drop-prefix)
                              symbol-drop
                              symbol-append)
                          (car (cddr x))
                          (to-id i))
                         (from-id i)))
                 (or (cdr mod-name+imports)
                     (module-exports (find-module (car mod-name+imports))))))))
   ((and (pair? (cdr x)) (pair? (cadr x)))
    (if (memq (car x) '(only except rename))
        (let* ((mod-name+imports (resolve-import (cadr x)))
               (imp-ids (or (cdr mod-name+imports)
                            (and (not (eq? 'only (car x)))
                                 (module-exports
                                  (find-module (car mod-name+imports)))))))
          ;; (if (not (eq? 'only (car x)))
          ;;     (let ((unbound
          ;;            (id-filter (lambda (i) (not (memq i imp-ids))) (cddr x))))
          ;;       (if (pair? unbound)
          ;;           (warn "import excepting unbound identifiers" unbound))))
          (cons (car mod-name+imports)
                (case (car x)
                  ((only)
                   (cddr x))
                  ((except)
                   (id-filter (lambda (i) (not (memq i (cddr x)))) imp-ids))
                  ((rename)
                   (map (lambda (i)
                          (let ((rename (assq (to-id i) (cddr x))))
                            (if rename (cons (cadr rename) (from-id i)) i)))
                        imp-ids)))))
        (error "invalid import modifier" x)))
   ((find-module x)
    => (lambda (mod) (cons x (%module-exports mod))))
   (else
    (error "couldn't find import" x))))

; untested
(define (eval-module name mod . o)
  (let ((env (if (pair? o) (car o) (make-environment)))
        (meta (module-meta-data mod))
        (dir (module-name-prefix name)))
    (define (load-modules files extension fold?)
      (for-each
       (lambda (f)
         (let ((f (string-append dir f extension)))
           (cond
            ((find-module-file f)
             => (lambda (path)
                  (cond (fold?
                         (let ((in (open-input-file path)))
                           (set-port-fold-case! in #t)
                           (load in env)))
                        (else
                         (load path env)))))
            (else (error "couldn't find include" f)))))
       files))
    ;; catch cyclic references
    (module-meta-data-set!
     mod
     `((error "module attempted to reference itself while loading" ,name)))
    (for-each
     (lambda (x)
       (case (and (pair? x) (car x))
         ((import import-immutable)
          (for-each
           (lambda (m)
             (let* ((mod2-name+imports (resolve-import m))
                    (mod2 (load-module (car mod2-name+imports))))
               (%import env (module-env mod2) (cdr mod2-name+imports) #t)))
           (cdr x)))))
     meta)
    (for-each
     (lambda (x)
       (case (and (pair? x) (car x))
         ((include)
          (load-modules (cdr x) "" #f))
         ((include-ci)
          (load-modules (cdr x) "" #t))
         ((include-shared)
          (load-modules (cdr x) *shared-object-extension* #f))
         ;((body begin) ; JAE - Restore this line, debugging using 'begin2'
         ((body begin2)
          (for-each 
            (lambda (expr) 
                (write (list "DEBUG" expr))
                (eval expr env)) 
            (cdr x)))
         ((error)
          (apply error (cdr x)))))
     meta)
    (module-meta-data-set! mod meta)
    ;(warn-undefs env #f) ; JAE - commented this out (TODO)
    env))

; JAE - Trying to figure out how this works
; Uses resolve-import to make sure each import is valid.
; For example:
;
;  huski> (load "modules.scm" )
;  huski> (resolve-import '(hello world))
;  ((hello world))
;  huski> (resolve-import '(hello world 1))
;  Error: File does not exist: hello/world/1.sld
;
; Then, based on that information it uses load-module to (load) the given module
; into a new environment that is returned and assigned to mod2
;
; Then this is imported into env somehow by the call to %import
; not quite sure how that works yet
(define (environment . ls)
  (let ((env (make-environment)))
    (for-each
     (lambda (m)
       (let* ((mod2-name+imports (resolve-import m))
              (mod2 (load-module (car mod2-name+imports))))
         (%import env (module-env mod2) (cdr mod2-name+imports) #t)))
     ls)
    env))

(define (load-module name)
  (let ((mod (find-module name)))
    (if (and mod (not (module-env mod)))
        (module-env-set! mod (eval-module name mod)))
    mod))

;TODO: see below:
;(define define-library-transformer
(define-syntax define-library
  (er-macro-transformer
   (lambda (expr rename compare)
     (let ((name (cadr expr))
           (body (cddr expr))
           (tmp (rename 'tmp))
           ; (this-module (rename '*this-module*)) ; TODO: this is the old binding that must be restored - Issue #99
           (this-module '*this-module*)
           (add-module! (rename 'add-module!)))
       `(let ((,tmp ,this-module))
          (define (rewrite-export x)
            (write (list "DEBUG: rewrite-export" x))
            (if (pair? x)
                (if (and (= 3 (length x))
                         (eq? 'rename (identifier->symbol (car x))))
                    (cons (car (cddr x)) (cadr x))
                    (error "invalid module export" x))
                x))
          (define (extract-exports)
            (write "entered extract-exports") ; JAE Debugging
            (write ,this-module) ; JAE Debugging
            (cond
             ((assq 'export-all ,this-module)
              => (lambda (x)
                   (if (pair? (cdr x))
                       (error "export-all takes no parameters" x))
                   #f))
             (else
              (let lp ((ls ,this-module) (res '()))
                (cond
                 ((null? ls) res)
                 ((and (pair? (car ls)) (eq? 'export (caar ls)))
                  (lp (cdr ls) (append (map rewrite-export (cdar ls)) res)))
                 (else (lp (cdr ls) res)))))))
          (set! ,this-module '())
          ,@body
          (set! ,this-module (reverse ,this-module))
          (,add-module! ',name (make-module (extract-exports) #f ,this-module))
          (set! ,this-module ,tmp))))))

; JAE TODO: chibi supports 'module' as well as 'define-library'
;(define-syntax define-library define-library-transformer)
;(define-syntax module define-library-transformer)

(define-syntax define-config-primitive
  (er-macro-transformer
   (lambda (expr rename compare)
     `(define-syntax ,(cadr expr)
        (er-macro-transformer
         (lambda (expr rename compare)
           ; (let ((this-module (rename '*this-module*))) ; JAE TODO: need to restore this line, see Issue #99
           (let ((this-module '*this-module*))
             `(set! ,this-module (cons ',expr ,this-module)))))))))

;JAE - TODO: (define-syntax orig-begin begin)
(define-config-primitive import)
(define-config-primitive import-immutable)
(define-config-primitive export)
(define-config-primitive export-all)
(define-config-primitive include)
(define-config-primitive include-ci)
(define-config-primitive include-shared)
(define-config-primitive body)
;(define-config-primitive begin) ; JAE - Restore this line, debugging using 'begin2'
(define-config-primitive begin2) ; JAE - Restore this line, debugging using 'begin2'

; The `import' binding used by (scheme) and (scheme base), etc.
(define-syntax repl-import
  (er-macro-transformer
   ;(let ((meta-env (current-environment)))
     (lambda (expr rename compare)
       (let lp ((ls (cdr expr)) (res '()))
         (cond
          ((null? ls)
;            (reverse res)) ; JAE - more workarounds
           (cons (rename 'begin) (reverse res))) ; JAE - this line is a temporary workaround
; JAE TODO:           (cons (rename 'orig-begin) (reverse res)))
          (else
           (let ((mod+imps (resolve-import (car ls))))
             (cond
              ((pair? mod+imps)
               (lp (cdr ls)
                   (cons `(,(rename '%import)
                           #f
                           (,(rename 'module-env)
                            (,(rename 'load-module)
                             (,(rename 'quote) ,(car mod+imps))))
                           (,(rename 'quote) ,(cdr mod+imps))
                           #f)
                         res)))
              (else
               (error "couldn't find module" (car ls)))))))))));)

; JAE - TODO:
;(define *modules*
;  (list (cons '(scheme) (make-module #f (interaction-environment)
;                                     '((include "init-7.scm"))))
;        (cons '(meta) (make-module #f (current-environment) '()))
;        (cons '(srfi 0) (make-module (list 'cond-expand)
;                                     (current-environment)
;                                     (list (list 'export 'cond-expand))))))

