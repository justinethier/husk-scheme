;;; 
;;; husk-scheme
;;; http://github.com/justinethier/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; Minimal unit testing framework
;;;

(define *file-with-counts* "scm-unit.tmp")
(define pass-count 0)
(define fail-count 0)

(define (unit-test-handler expected actual . label) 
  (if (not (equal? expected actual))
    (begin 
        (display "Test ")
        (if (not (null? (car label))) 
            (begin
                (display "[")
                (display (caar label))
                (display "] ")))
        (display "failed; expected value: [")
        (display expected) 
        (display "], actual value: [") 
        (display actual)
        (display "]")
        (newline)
        (set! fail-count (+ fail-count 1)))
    (set! pass-count (+ pass-count 1))))

(define (assert proc) (unit-test-handler #t (proc)))

;(define (assert-equal proc value) (unit-test-handler value (proc)))

(define (assert/equal test expected . label)
     (unit-test-handler 
        expected 
        ((lambda () test))
        label))

(define (unit-test-start name)
  (display "Testing ")
  (display name)
  (display "... ")
  (newline))

(define (unit-test-handler-results)
;  (display "Test complete!")
  (display " Passed: ")
  (display pass-count)
  (display " Failed: ")
  (display fail-count)
  (newline)
  (newline)

  (define total-pass 0)
  (define total-fail 0)
  (let ((inf (open-input-file *file-with-counts*)))
      (set! total-pass (read inf))
      (set! total-fail (read inf))
      (close-input-port inf))
  
  (let ((outf (open-output-file *file-with-counts*)))
      (write (+ total-pass pass-count) outf)
      (write (+ total-fail fail-count) outf)
      (close-output-port outf))

  (set! pass-count 0)
  (set! fail-count 0))

(define (summarize-results)
  (define total-pass 0)
  (define total-fail 0)
  (let ((inf (open-input-file *file-with-counts*)))
      (set! total-pass (read inf))
      (set! total-fail (read inf))
      (close-input-port inf))
  (display "------------------------------------------")
  (newline)
  (display "All tests complete! ")
  (newline)
  (display "Total Passed: ")
  (display total-pass)
  (newline)
  (display "Total Failed: ")
  (display total-fail)
  (newline))
