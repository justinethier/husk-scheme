# greater
./skim "(cond ((> 3 2) 'greater) ((< 3 2) 'less))" 
# equal
./skim "(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))"

# TODO: => special form
#./skim "(cond ((assv 'b '((a 1) (b 2))) => cadr)
#                        (else #f))                 ===>  2
