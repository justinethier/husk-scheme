; This example proves that a Scheme implements proper tail recursion (or tail call optimization, or TCO).
;
; TCO isn't just about self-calling. The following is expected to run forever in scheme. It will use ~100% CPU, however memory usage is expected to remain constant and a stack overflow should never occur:
;
(define (foo) (bar))
(define (bar) (foo))
(foo)
