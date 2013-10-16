(import 
    (scheme base)
    (scheme write))
(load "skim-unit.scm")
(newline)
(display "------------------------------------------")
(newline)
(display "Running R7RS test suite")
(newline)
(newline)
(define p (open-output-file "scm-unit.tmp"))
(display "0\n0\n" p)
(close-output-port p)
; r7rs libraries
(let () (load "t-libs.scm"))
; Summarize test results
(let () (load "summarize.scm"))
(delete-file "scm-unit.tmp")
