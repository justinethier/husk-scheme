#! /usr/bin/env huski
;
; This is the example code from SRFI-22
; http://srfi.schemers.org/srfi-22/srfi-22.html
;
; A clone of the UNIX cat command, written in scheme.
; To run from the command line: 
;
;   ./cat.scm filename
;
(define (main arguments)
  (for-each display-file (cdr arguments))
  0)

(define (display-file filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ()
    (let ((thing (read-char port)))
      (if (not (eof-object? thing))
          (begin
        (write-char thing)
        (loop))))))))
