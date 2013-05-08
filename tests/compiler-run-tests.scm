(load "skim-unit.scm")

(define p (open-output-file "scm-unit.tmp"))
(display "0\n0\n" p)
(close-output-port p)
; Standard tests
;(let () (load "t-backquote.scm")) ; Cons error, probably in er-macro handling
(let () (load "t-case.scm"))
(let () (load "t-closure.scm"))
(let () (load "t-cond.scm"))
(let () (load "t-cont.scm"))
(let () (load "t-delay.scm"))
(let () (load "t-eval.scm"))
(let () (load "t-exec.scm"))
(let () (load "t-extensions.scm"))
(let () (load "t-hashtable.scm"))
(let () (load "t-io.scm"))
(let () (load "t-iteration.scm"))
(let () (load "t-numerical-ops.scm"))
(let () (load "t-parser.scm"))
;(let () (load "t-scoping.scm")) ; "Not implemented" error
(let () (load "t-special-forms.scm"))
(let () (load "t-standard-procedures.scm"))
;(let () (load "t-stdlib.scm")) ; Variable 'bar' not defined error
(let () (load "t-storage.scm"))
(let () (load "t-string.scm"))
;(let () (load "t-vector.scm")) ; Unbound variable error, looks macro related
(let () (load "t-bytevector.scm"))
;; Macro tests
(let () (load "t-macro.scm"))
(let () (load "t-macro-lists.scm"))
(let () (load "t-macro-hygiene.scm"))
(let () (load "t-macro-ref-trans.scm"))
(let () (load "t-er-macro.scm"))
;; SRFI tests
;(let () (load "t-srfi-1.scm")) ; There is a *lot* of code here, may be difficult to test or have long compiles
;(let () (load "t-srfi-2.scm")) ; Get a variable 'and-let*' not defined error
;; Summarize test results
(let () (load "summarize.scm"))
(delete-file "scm-unit.tmp")
