(define-syntax when
(syntax-rules ()
((when condition . body) (if condition (begin . body) #f))))
;((when condition body ...) (if condition (begin . body) #f))))

(define x -1)
(when (negative? x)
      (newline)
      (display "bad number: negative"))

#|
Output when run with original version of above:
huski> (load "scm-unit-tests/test
             test.scm   test2.scm
             huski> (load "scm-unit-tests/test.scm" )
             loadLocal pattern = ((condition . body)) input = ((negative? x) (newline) (display "bad number: negative"))
             loadLocal pattern = (condition . body) input = (negative? x)
             loadLocal pattern = (condition body ...) input = (negative? x)
             loadLocal pattern = (body ...) input = (x)
             loadLocal pattern = (body ...) input = ()
             loadLocal pattern = () input = ((newline) (display "bad number: negative"))
             Input does not match a macro pattern: (when (negative? x) (newline) (display "bad number: negative"))

Output when using the ... above instead of a pair:
huski> (load "scm-unit-tests/test.scm" )
loadLocal pattern = (condition body ...) input = ((negative? x) (newline) (display "bad number: negative"))
loadLocal pattern = (body ...) input = ((newline) (display "bad number: negative"))
loadLocal pattern = (body ...) input = ((display "bad number: negative"))
loadLocal pattern = (body ...) input = ()

"bad number: negative"

Can clearly see from above that the problem is that by the time we get to inserting the ...
in the pattern, we have already moved too far into the input. Perhaps the pattern matcher
needs to search for the pattern List[DottedList] instead of searching for dotted lists directly.
Need to be careful here...
|#

#|
(load "../stdlib.scm")

;; TODO: the problem here is that these cases have an improper list in the pattern and a proper list in the transform
;; the passing cases had improper lists in both.
;;
;; The only issue here is that we are currently appending '() to the lists in the pattern. this is going to be 
;; problematic, may need to consider using an external data structure to store a boolean flag, rather than 
;; appending the data to the pattern/input data
(write "TODO: the following are not transformed correctly by husk:")

; Would be surprized if this worked in husk at the moment...
;(write (pair-test (1 2 . )))
;;(1 2)

#| TODO: output from csi:
(1 2 3)
(1 2 (3))
(1 2 (3 4))
(1 2 ())
(1 2 (3 4 5 . 6))
((1 2 3))
((1 2 (3)))
((1 2 ()))
|#

; A temporary test to demonstrate what is happening in the other tests below.
; We do not handle the case where a variable is in an nary match in the pattern but is
; used directly in the transform. This should certainly be an error in some cases such
; as this one. But the pair/list ones below are real an absolutely must be handled.
(define-syntax test
  (syntax-rules ()
     ((_ (a b c ...))
      (quote (a b c)))))
(write (test (1 2)))

; A macro taking a pair in the pattern and transforming into a list
(define-syntax pair-test-00
  (syntax-rules ()
     ((_ (a b . c))
      (quote (a b c)))))
(write (pair-test-00 (1 2 . 3)))
;(1 2 3)

(write (pair-test-00 (1 2 3)))
;(1 2 (3))

(write (pair-test-00 (1 2 3 4)))
;(1 2 (3 4))
(write (pair-test-00 (1 2)))
;(1 2 ())
(write (pair-test-00 (1 2 3 4 5 . 6)))
(write (pair-test-00 (1 2 3 . 6)))
;(1 2 (3 4 5 . 6))

; Same as previous macro, but packaged using (list) - any real difference here?
(define-syntax pair-test-01
  (syntax-rules ()
     ((_ (a b . c))
      (list (quote (a b c))))))

(write (pair-test-01 (1 2 . 3)))
; ((1 2 3))

(write (pair-test-01 (1 2 3)))
; ((1 2 (3)))

(write (pair-test-01 (1 2)))
;((1 2 ()))
;
; TODO: test with this macro:
;
;(define-syntax pair-test-00
;  (syntax-rules ()
;     ((_ (a b . ((c))))
;      (quote (a b c)))))
|#
