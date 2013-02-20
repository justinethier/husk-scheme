;; Main program.
(import (r5rs base)
        (only (example life) life)
        (rename (prefix (example grid) grid-)
                (grid-make make-grid)))

;; Initialize a grid with a glider.
(define grid (make-grid 24 24))
;(grid-set! grid 1 1 #t)
;(grid-set! grid 2 2 #t)
;(grid-set! grid 3 0 #t)
;(grid-set! grid 3 1 #t)
;(grid-set! grid 3 2 #t)
(grid-put! grid 1 1 #t)
(grid-put! grid 2 2 #t)
(grid-put! grid 3 0 #t)
(grid-put! grid 3 1 #t)
(grid-put! grid 3 2 #t)
;; Run for 80 iterations.
;; JAE - suspect this is so slow because macros are expanded
;; each time. need to confirm and find some way to memoize
;; expansions
(life grid 10) ; TODO: 80)
