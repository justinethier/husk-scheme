; A simple example to clear a VT100 terminal
(display
    (string 
        (integer->char #x1B) 
        #\[
        #\H
        (integer->char #x1B)
        #\[
        #\J))
