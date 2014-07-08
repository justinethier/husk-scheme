;; Works in huskc:
;; (define-syntax my-let (syntax-rules ()                                    
;;     ((my-let x)                                                           
;;       (begin x))))                                                        
;;                                                                           
;; (write (eval '(my-let 2)))                                                

;; Does not work yet:                                                 
(let-syntax ((my-let (syntax-rules ()                                        
    ((my-let x)                                                              
      (begin x)))))                                                          
  (write (eval '(my-let 2)))) 
