;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for cond-expand form
;;
(unit-test-start "cond-expand")

(assert/equal 
    (cond-expand ((and) 'test))
    'test)
(assert/equal 
    (cond-expand ((or) 'test))
    #t)
(assert/equal 
    (cond-expand ((and r7rs) 'test))
    'test)
(assert/equal 
    (cond-expand ((and r7rs bogus) 'test))
    #t)
(assert/equal 
    (cond-expand ((or r7rs bogus) 'test))
    'test)

(unit-test-handler-results)
