; From: http://sisc-scheme.org/r5rs_pitfall.php
;
; This file may be run directly from the command line:
;  
;   ./huski scm-unit-tests/r5rs_pitfall.scm
;
; File has been modified to comment-out cases that do not run in husk.
; If a case is not listed in the output then assume it fails.
;
;
;; r5rs_pitfalls.scm
;; 
;; This program attempts to test a Scheme implementation's conformance
;; to various subtle edge-cases and consequences of the R5RS Scheme standard.
;; Code was collected from public forums, and is hereby placed in the public domain.
;;
;; 
(define-syntax should-be
  (syntax-rules ()
    ((_ test-id value expression)
     (let ((return-value expression))
         (if (not (equal? return-value value))
           (for-each (lambda (v) (display v))
                     `("Failure: " test-id ", expected '"
                     value "', got '" ,return-value "'." #\newline))
           (for-each (lambda (v) (display v))
                     '("Passed: " test-id #\newline)))))))

(define call/cc call-with-current-continuation)

;; Section 1: Proper letrec implementation

;;Credits to Al Petrofsky
;; In thread:
;; defines in letrec body 
;; http://groups.google.com/groups?selm=87bsoq0wfk.fsf%40app.dial.idiom.com
(should-be 1.1 0
 (let ((cont #f))
   (letrec ((x (call-with-current-continuation (lambda (c) (set! cont c) 0)))
            (y (call-with-current-continuation (lambda (c) (set! cont c) 0))))
     (if cont
         (let ((c cont))
           (set! cont #f)
           (set! x 1)
           (set! y 1)
           (c 0))
         (+ x y)))))

;;Credits to Al Petrofsky
;; In thread:
;; Widespread bug (arguably) in letrec when an initializer returns twice
;; http://groups.google.com/groups?selm=87d793aacz.fsf_-_%40app.dial.idiom.com
(should-be 1.2 #t
  (letrec ((x (call/cc list)) (y (call/cc list)))
    (cond ((procedure? x) (x (pair? y)))
	  ((procedure? y) (y (pair? x))))
    (let ((x (car x)) (y (car y)))
      (and (call/cc x) (call/cc y) (call/cc x)))))

;;Credits to Alan Bawden
;; In thread:
;; LETREC + CALL/CC = SET! even in a limited setting 
;; http://groups.google.com/groups?selm=19890302162742.4.ALAN%40PIGPEN.AI.MIT.EDU
(should-be 1.3 #t
  (letrec ((x (call-with-current-continuation
		  (lambda (c)
		    (list #t c)))))
      (if (car x)
	  ((cadr x) (list #f (lambda () x)))
	  (eq? x ((cadr x))))))

;; Section 2: Proper call/cc and procedure application

;;Credits to Al Petrofsky, (and a wink to Matthias Blume)
;; In thread:
;; Widespread bug in handling (call/cc (lambda (c) (0 (c 1)))) => 1 
;; http://groups.google.com/groups?selm=87g00y4b6l.fsf%40radish.petrofsky.org
(should-be 2.1 1
 (call/cc (lambda (c) (0 (c 1)))))

;; Section 3: Hygienic macros

;; Eli Barzilay 
;; In thread:
;; R5RS macros...
;; http://groups.google.com/groups?selm=skitsdqjq3.fsf%40tulare.cs.cornell.edu
(should-be 3.1 4
  (let-syntax ((foo
                (syntax-rules ()
                  ((_ expr) (+ expr 1)))))
    (let ((+ *))
      (foo 3))))


;; Al Petrofsky again
;; In thread:
;; Buggy use of begin in r5rs cond and case macros. 
;; http://groups.google.com/groups?selm=87bse3bznr.fsf%40radish.petrofsky.org
(should-be 3.2 2
 (let-syntax ((foo (syntax-rules ()
                       ((_ var) (define var 1)))))
     (let ((x 2))
       (begin (define foo +))
       (cond (else (foo x))) 
       x)))

;;Al Petrofsky
;; In thread:
;; An Advanced syntax-rules Primer for the Mildly Insane
;; http://groups.google.com/groups?selm=87it8db0um.fsf@radish.petrofsky.org

(should-be 3.3 1
  (let ((x 1))
    (let-syntax
        ((foo (syntax-rules ()
                ((_ y) (let-syntax
                             ((bar (syntax-rules ()
                                   ((_) (let ((x 2)) y)))))
                         (bar))))))
      (foo x))))

;; Al Petrofsky
;; Contributed directly
(should-be 3.4 1
  (let-syntax ((x (syntax-rules ()))) 1))

;; Setion 4: No identifiers are reserved

;;(Brian M. Moore)
;; In thread:
;; shadowing syntatic keywords, bug in MIT Scheme?
;; http://groups.google.com/groups?selm=6e6n88%248qf%241%40news.cc.ukans.edu
(should-be 4.1 '(x)
 ((lambda lambda lambda) 'x))

(should-be 4.2 '(1 2 3)
 ((lambda (begin) (begin 1 2 3)) (lambda lambda lambda)))

(should-be 4.3 #f
 (let ((quote -)) (eqv? '1 1)))
;; Section 5: #f/() distinctness

;; Scott Miller
(should-be 5.1 #f
  (eq? #f '()))
(should-be 5.2 #f
  (eqv? #f '()))
(should-be 5.3 #f
  (equal? #f '()))

;; Section 6: string->symbol case sensitivity

;; Jens Axel S?gaard
;; In thread:
;; Symbols in DrScheme - bug? 
;; http://groups.google.com/groups?selm=3be55b4f%240%24358%24edfadb0f%40dspool01.news.tele.dk
(should-be 6.1 #f
  (eq? (string->symbol "f") (string->symbol "F")))

;; Section 7: First class continuations

;; Scott Miller
;; No newsgroup posting associated.  The gist of this test and 7.2
;; is that once captured, a continuation should be unmodified by the 
;; invocation of other continuations.  This test determines that this is 
;; the case by capturing a continuation and setting it aside in a temporary
;; variable while it invokes that and another continuation, trying to 
;; side effect the first continuation.  This test case was developed when
;; testing SISC 1.7's lazy CallFrame unzipping code.
(define r #f)
(define a #f)
(define b #f)
(define c #f)
(define i 0)
(should-be 7.1 28
  (let () 
    (set! r (+ 1 (+ 2 (+ 3 (call/cc (lambda (k) (set! a k) 4))))
               (+ 5 (+ 6 (call/cc (lambda (k) (set! b k) 7))))))
    (if (not c) 
        (set! c a))
    (set! i (+ i 1))
    (case i
      ((1) (a 5))
      ((2) (b 8))
      ((3) (a 6))
      ((4) (c 4)))
    r))

;; Same test, but in reverse order
(define r #f)
(define a #f)
(define b #f)
(define c #f)
(define i 0)
(should-be 7.2 28
  (let () 
    (set! r (+ 1 (+ 2 (+ 3 (call/cc (lambda (k) (set! a k) 4))))
               (+ 5 (+ 6 (call/cc (lambda (k) (set! b k) 7))))))
    (if (not c) 
        (set! c a))
    (set! i (+ i 1))
    (case i
      ((1) (b 8))
      ((2) (a 5))
      ((3) (b 7))
      ((4) (c 4)))
    r))

;; Credits to Matthias Radestock
;; Another test case used to test SISC's lazy CallFrame routines.
(should-be 7.3 '((-1 4 5 3)
                 (4 -1 5 3)
                 (-1 5 4 3)
                 (5 -1 4 3)
                 (4 5 -1 3)
                 (5 4 -1 3))
  (let ((k1 #f)
        (k2 #f)
        (k3 #f)
        (state 0))
    (define (identity x) x)
    (define (fn)
      ((identity (if (= state 0)
                     (call/cc (lambda (k) (set! k1 k) +))
                     +))
       (identity (if (= state 0)
                     (call/cc (lambda (k) (set! k2 k) 1))
                     1))
       (identity (if (= state 0)
                     (call/cc (lambda (k) (set! k3 k) 2))
                     2))))
    (define (check states)
      (set! state 0)
      (let* ((res '())
             (r (fn)))
        (set! res (cons r res))
        (if (null? states)
            res
            (begin (set! state (car states))
                   (set! states (cdr states))
                   (case state
                     ((1) (k3 4))
                     ((2) (k2 2))
                     ((3) (k1 -)))))))
    (map check '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))))

;; Modification of the yin-yang puzzle so that it terminates and produces
;; a value as a result. (Scott G. Miller)
(should-be 7.4 '(10 9 8 7 6 5 4 3 2 1 0)
  (let ((x '())
        (y 0))
    (call/cc 
     (lambda (escape)
       (let* ((yin ((lambda (foo) 
                      (set! x (cons y x))
                      (if (= y 10)
                          (escape x)
                          (begin
                            (set! y 0)
                            foo)))
                    (call/cc (lambda (bar) bar))))
              (yang ((lambda (foo) 
                       (set! y (+ y 1))
                       foo)
                     (call/cc (lambda (baz) baz)))))
         (yin yang))))))

;; Miscellaneous 

;;Al Petrofsky
;; In thread:
;; R5RS Implementors Pitfalls
;; http://groups.google.com/groups?selm=871zemtmd4.fsf@app.dial.idiom.com
(should-be 8.1 -1
  (let - ((n (- 1))) n))

(should-be 8.2 '(1 2 3 4 1 2 3 4 5)
  (let ((ls (list 1 2 3 4)))
    (append ls ls '(5))))

;; This example actually illustrates a bug in R5RS.  If a Scheme system
;; follows the letter of the standard, 1 should be returned, but
;; the general agreement is that 2 should instead be returned.
;; The reason is that in R5RS, let-syntax always introduces new scope, thus 
;; in the following test, the let-syntax breaks the definition section
;; and begins the expression section of the let. 
;;
;; The general agreement by the implementors in 1998 was that the following 
;; should be possible, but isn't:
;;
;;   (define ---)
;;   (let-syntax (---)
;;     (define ---)
;;     (define ---))
;;   (define ---)
;;
;; Scheme systems based on the Portable syntax-case expander by Dybvig
;; and Waddell do allow the above, and thus often violate the letter of
;; R5RS.  In such systems, the following will produce a local scope:
;;
;;   (define ---)
;;   (let-syntax ((a ---))
;;     (let ()
;;       (define ---)
;;       (define ---)))
;;   (define ---)
;;
;; Credits to Matthias Radestock and thanks to R. Kent Dybvig for the
;; explanation and background
(should-be 8.3 1
  (let ((x 1))
    (let-syntax ((foo (syntax-rules () ((_) 2))))
      (define x (foo))
      3)
    x))

;;Not really an error to fail this (Matthias Radestock)
;;If this returns (0 1 0), your map isn't call/cc safe, but is probably
;;tail-recursive.  If its (0 0 0), the opposite is true.
(let ((result 
       (let ()
         (define executed-k #f)
         (define cont #f)
         (define res1 #f)
         (define res2 #f)
         (set! res1 (map (lambda (x)
                           (if (= x 0)
                               (call/cc (lambda (k) (set! cont k) 0))
                               0))
                         '(1 0 2)))
         (if (not executed-k)           
             (begin (set! executed-k #t) 
                    (set! res2 res1)
                    (cont 1)))
         res2)))
  (if (equal? result '(0 0 0))
      (display "Map is call/cc safe, but probably not tail recursive or inefficient.")
      (display "Map is not call/cc safe, but probably tail recursive and efficient."))
  (newline))

