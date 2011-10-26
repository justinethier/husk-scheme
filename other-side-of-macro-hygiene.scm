; The purpose of this file is to find a working example or two illustrating the 'other' side of hygiene
(define-syntax cond
  (syntax-rules
    ((cond) => #f)
    ((cond (else ?result ...) ?clause ...)
     => (begin ?result ...))
    ((cond (?test) ?clause ...)
     => (or ?test (cond ?clause ...)))
    ((cond (?test ?result ...) ?clause ...)
     => (if ?test
          (begin ?result ...)
          (cond ?clause ...))))

  (let ((else #f))
    (cond (#f 3)
          (else 4)
          (#t 5)))
