(define-syntax orr 
  (syntax-rules () 
   ((orelse <expr1> <expr2>) 
    (let ((temp <expr1>)) 
         (if temp temp <expr2>)))))

(write
    (let ((temp 4)) (orr #f temp)))
;=> #f instead of 4

(write
    (let ((if +)) (orr 1 1)))
;=> 1 (correct)

(write
    (let ((if +)) (if (orr 1 1) 10 100)))
;=> 10 instead of 111
