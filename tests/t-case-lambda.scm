;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for case-lambda form
;;
(unit-test-start "case-lambda")

; Tests from spec
(begin
  (import (scheme case-lambda))
  (define range
    (case-lambda
      ((e) (range 0 e))
      ((b e) (do ((r '() (cons e r))
                  (e (- e 1) (- e 1)))
                 ((< e b) r)))))
  (assert/equal (range 3) '(0 1 2))
  (assert/equal (range 3 5) '(3 4))
)

(unit-test-handler-results)
