; Example of using continuations to implement co-routines (green threads)
; Code obtained from: http://en.wikipedia.org/wiki/Continuation
;
; This file is still a work-in-progress and does not work correctly
; from husk.
;
;;; A naive queue for thread scheduling.
;;; It holds a list of continuations "waiting to run".

(define *queue* '())

(define (empty-queue?)
  (null? *queue*))

(define (enqueue x)
  (set! *queue* (append *queue* (list x))))

(define (dequeue)
  (let ((x (car *queue*)))
    (set! *queue* (cdr *queue*))
    x))

;;; This starts a new thread running (proc).

(define (fork proc)
  (call/cc
    (lambda (k)
      (enqueue k)
      (proc))))

;;; This yields the processor to another thread, if there is one.

(define (yield)
  (call/cc
    (lambda (k)
      (enqueue k)
      ((dequeue)))))

;;; This terminates the current thread, or the entire program
;;; if there are no other threads left.

(define (thread-exit)
  (if (empty-queue?)
    (exit)
    ((dequeue))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The body of some typical Scheme thread that does stuff:

(define (do-stuff-n-print str)
  (lambda ()
    (let loop ((n 0))
      (write (list str n))
      (yield)
      (loop (+ 1 n)))))
;      (format #t "~A ~A\n" str n)

;;; Create two threads, and start them running.
(fork (do-stuff-n-print "This is AAA"))
(fork (do-stuff-n-print "Hello from BBB"))
(thread-exit)
