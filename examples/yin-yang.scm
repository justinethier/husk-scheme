;;; 
;;; Justin Ethier
;;; husk-scheme
;;;
;;; Example program for First class continuations, from
;;; http://en.wikipedia.org/wiki/Scheme_(programming_language)#First-class_continuations
;;;
(let* ((yin
        ((lambda (cc) (display "@") cc) (call-with-current-continuation (lambda (c) c))))
       (yang
        ((lambda (cc) (display "*") cc) (call-with-current-continuation (lambda (c) c)))))
      (yin yang))
