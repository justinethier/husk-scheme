
(define-syntax test
 (syntax-rules ()
   ((_ if)
    (if #t #t #f))))

; Appears func is called here...
; presumably because the let's if is not in the env when test is expanded?
(write
  (let ((if (lambda (x y z) "func is called"))) (test if)))

; In this case, if should be taken from the def environment (IE, the special form) and
; not from the use environment (which is the "should not happen" function.
;
; BUT, I'm confused, why does the macro match at all?
(define if (lambda (x y z) "should not happen"))
(write
  (test if))
