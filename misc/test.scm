; From: 
; http://groups.google.com/group/comp.lang.scheme/msg/eb6cc6e11775b619?pli=1
#| TODO: add this to the macro test suite, expected value: (1 2 3 a)
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
|#
(write
  (let ((x 1))
    (let-syntax
        ((foo (syntax-rules ()
                ((_ y) (let-syntax
                             ((bar (syntax-rules ()
                                   ((_) (let ((x 2)) y)))))
                         (bar))))))
      (foo x))))
