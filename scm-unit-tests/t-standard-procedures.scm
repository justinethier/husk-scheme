(load "skim-unit.scm")

; Adding test cases from:
; http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_chap_6
;
(assert/equal
  (string=? (symbol->string obj1)
            (symbol->string obj2))
  #t)

