(load "skim-unit.scm")

(assert/equal
  (string=? (symbol->string obj1)
            (symbol->string obj2))
  #t)

