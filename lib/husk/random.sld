(define-library (husk random)
    (export 
        random
        randint)
    (import (scheme r5rs))
    (include "random.scm"))

