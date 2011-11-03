
; A demonstration of how to deal with 'other side of hygiene' when
; the literal is in the template
(define v 1)
(define-syntax test-template
 (syntax-rules (v)
   ((_)
    v)))

(write (test-template))
(write (let ((v 1)) (test-template)))
(write (let ((v 2)) (test-template)))
(define v 3)
(write (test-template))

#|
(define var 'original)

(define-syntax test-var
 (syntax-rules (var)
   ((_ var)
    var)
   ((_ pattern-var)
    'no-match)))

; Should return var since var is same in env/use 
(write (test-var var))

; Should be unmatched because of the difference between env/use
(write (let ((var 1)) (test-var var)))

; Again, should be unmatched because of the difference between env/use
(define var 'new-var)
(write (test-var var))
|#

; TODO: test the rewrite side by placing the literal in a template



#|
; if marked as a literal identifier so it is not loaded as a pattern variable...
(define-syntax test
 (syntax-rules (if)
   ((_ if)
    (if #t #t #f))
   ((_ a)
    'no-match)))

; if is introduced by (let) so it should not match in the macro because
; it is defined differently in env(use) and env(def):
(write
  (let ((if (lambda (x y z) "func is called"))) (test if)))

; But here there is no definition in env(use) so it does match the first rule
(write
  (test if))


; In this case, if should be taken from the def environment (IE, the special form) and
; not from the use environment (which is the "should not happen" function.
;
; BUT, I'm confused, why does the macro match at all?
(define if (lambda (x y z) "should not happen"))
;(define if 1)
(write
  (test if))

; Since the new if is now in the def environment, it should execute the function this time
(define-syntax test-2
 (syntax-rules (if)
   ((_ if)
    (if #t #t #f))))
(write
  (test-2 if))
|#
