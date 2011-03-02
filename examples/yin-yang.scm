(let* ((yin
        ((lambda (cc) (display "@") cc) (call-with-current-continuation (lambda (c) c))))
       (yang
        ((lambda (cc) (display "*") cc) (call-with-current-continuation (lambda (c) c)))))
      (yin yang))
