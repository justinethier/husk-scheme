;; The purpose of this file is to find a working example or two illustrating the 'other' side of hygiene

; TODO: the test case below actually returns the expected result (5) in huski.
; Need to figure out if it does so for the right reasons (unlikely because it is not implemented per Clinger)
; or if (more likely) it just happens to be a coincidence.
;
; Anyway, need to figure out why the below works, and why it is *supposed* to work per the paper
;

;(define-syntax cond
;  (syntax-rules
;    ((cond) => #f)
;    ((cond (else ?result ...) ?clause ...)
;     => (begin ?result ...))
;    ((cond (?test) ?clause ...)
;     => (or ?test (cond ?clause ...)))
;    ((cond (?test ?result ...) ?clause ...)
;     => (if ?test
;          (begin ?result ...)
;          (cond ?clause ...)))))

; cond
; Form from R5RS:
(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

; This is an example from Macros That Work
(write
  (let ((else #f))
    (cond (#f 3)
          (else 4)
          (#t 5)))
  )
