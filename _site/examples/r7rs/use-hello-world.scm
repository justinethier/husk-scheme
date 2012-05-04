; A simple example of using an R7RS module
; Note: husk does not support this yet!
(import (scheme base)
        (scheme write)
        (hello world))
(write hello)
(newline)
