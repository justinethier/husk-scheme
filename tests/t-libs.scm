;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for r7rs libraries
;;
(unit-test-start "r7rs library syntax")

(begin
    (define x 'main-program)
    (import (only (libs basic) list2))
    (assert/equal x 'main-program "x not exported")
    (assert/equal (list2 1 2 3) '(1 2 3) "list2 exported"))

; TODO: not sure why the following fails?
;(begin
;    (define x 'main-program)
;    (import (libs basic))
;    (assert/equal x 'libs-basic "x exported"))

(begin
    (define x 'main-program)
    ; Demonstrate that multiple imports are OK
    (import (libs lib1)
            (only (libs basic) list2))
    (assert/equal x 'main-program "x not exported")
    (assert/equal (list2 1 2 3) '(1 2 3) "list2 exported"))
(begin
    (import (libs include))
    (assert/equal (raw) 'a-raw-included-file "include test #1"))

(unit-test-handler-results)
