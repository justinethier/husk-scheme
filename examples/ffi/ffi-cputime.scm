;;; 
;;; husk-scheme
;;; http://github.com/justinethier/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; Example code for how to load a husk plugin into the interpreter, so
;;; that arbitrary Haskell code may be called directly from Scheme.
;;; 
(load-ffi "Language.Scheme.Plugins.CPUTime" "precision" "cpu-time:precision")
(load-ffi "Language.Scheme.Plugins.CPUTime" "get" "cpu-time:get")

(write (cpu-time:precision))
(write (cpu-time:get))
(display "Seconds of CPU time spent: ")
(display (exact->inexact (/ (cpu-time:get) 1000000000000)))
(newline)
