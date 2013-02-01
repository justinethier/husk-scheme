; A simple example of using an R7RS module
; Note: husk does not support this yet!

; TODO: should be able to use the three lines below
;(import (scheme base)
;        (scheme write)
;        (hello world))
; But instead, we debug using these:
(load "../../lib/modules.scm")
(repl-import (hello world))
; end import
(write hello)
(newline)
