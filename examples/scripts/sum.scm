#!/usr/bin/env huski
#|
An example shell script using huski, that sums up numbers entered on
the command line. EG:

    ./sum.scm 0 1 1 2 3 5 8 13 21 34 55 89 144
|#
(define (main args)
    (write (apply + (map string->number (cdr args)))))
