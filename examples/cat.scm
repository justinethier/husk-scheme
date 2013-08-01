#! /usr/bin/env huski

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
