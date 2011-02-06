;
; husk-scheme
; http://github.com/justinethier/husk-scheme
;
; Written by Justin Ethier
;
; Test cases for quasi-quotation (back-quoting) 
;
(load "skim-unit.scm")

(assert/equal `(list ,(+ 1 2) 4)
	      '(list 3 4))

(assert/equal
   (list `("test complete" "passed" ,1))
  '(("test complete" "passed" 1)))

; Test case from R5RS
(assert/equal (let ((name 'a)) `(list ,name ',name))
              '(list a (quote a)))

(assert/equal (let ((name 'a)) `(list ,name (,name)))
              '(list a (a)))

(assert/equal (let ((name 'a)) `(list ,name ((,name))))
              '(list a ((a))))

; Ensure dotted lists are handled correctly
(assert/equal (let ((name 'a)) `(list ,name . ,name))
              '(list a . a))

(assert/equal (let ((name 'a)) `(list ,name . (,name)))
              '(list a a))

(assert/equal (let ((name 'a)) `(list ,name . ((,name))))
              '(list a (a)))

(assert/equal (let ((name 'a)) `(list ,name . (,name . ,name)))
              '(list a a . a))

(assert/equal `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
              '(a 3 4 5 6 b))

(assert/equal `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
              '((foo 7) . cons))

(assert/equal `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)
              '#(10 5 2.0 4.0 3.0 8))

;; In csi, the following is allowed, need to handle this correctly, where a scalar is supplied.
;; For now we are not allowing this.
;(assert/equal `(,@2)
;              2)

(assert/equal `(1 2 . ,(list 3 4))
              '(1 2 3 4))


; TODO: nested forms test cases from spec: 
;(assert/equal `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
;              '(a (quasiquote (b (unquote (+ 1 2)) (unquote (foo 4 d)) e)) f))

; TODO:
;(assert/equal (let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))
;             '(a `(b ,x ,'y d) e))


; Final set of test cases from spec:
(assert/equal (quasiquote (list (unquote (+ 1 2)) 4))
             '(list 3 4))

(assert/equal '(quasiquote (list (unquote (+ 1 2)) 4))
              '`(list ,(+ 1 2) 4))

(unit-test-handler-results)

