; From: 
; http://groups.google.com/group/comp.lang.scheme/msg/eb6cc6e11775b619?pli=1
(let ((a 1)) 
     (letrec-syntax 
         ((foo (syntax-rules () 
                 ((_ b) 
                  (bar a b)))) 
          (bar (syntax-rules () 
                 ((_ c d) 
                  (cons c (let ((c 3)) 
                            (list d c 'c))))))) 
       (let ((a 2)) 
         (foo a)))) 
