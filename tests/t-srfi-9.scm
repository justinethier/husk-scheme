;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for SRFI-9 (Record Types)
;;
(unit-test-start "SRFI 9: Record Types")
(define-record-type :pare
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))

(assert/equal (pare? (kons 1 2)) #t)
(assert/equal (pare? (cons 1 2)) #f)
(assert/equal (kar (kons 1 2))   1)
(assert/equal (kdr (kons 1 2))   2)
(assert/equal (let ((k (kons 1 2)))
                (set-kar! k 3)
                (kar k))
              3)
(unit-test-handler-results)
