;;;
;;; Justin Ethier
;;; husk scheme
;;;
;;; The game of life example from r7rs. 
;;; Main program
;;;
;;; To execute from the husk directory:
;;;
;;;  > cd examples/game-of-life
;;;  > huski -r7 life.scm
;;;
;;; Note the '-r7' flag is required in order to use R7RS scheme, with
;;; full library support.
;;;
(import (scheme base)
        (only (example life) life)
        (rename (prefix (example grid) grid-)
                (grid-make make-grid)))

;; Initialize a grid with a glider.
(define grid (make-grid 24 24))
(grid-put! grid 1 1 #t)
(grid-put! grid 2 2 #t)
(grid-put! grid 3 0 #t)
(grid-put! grid 3 1 #t)
(grid-put! grid 3 2 #t)
;; Run for x iterations.
(life grid 10) ;80
