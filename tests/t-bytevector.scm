;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for bytevectors
;;
(unit-test-start "bytevectors")

(assert/equal (bytevector? #u8(1 2 3 4 5)) #t)
(assert/equal (bytevector? '#(1 2 3 4 5)) #f)

(assert/equal 
    (make-bytevector 2 12)
    #u8(12 12))

(assert/equal (bytevector 1 3 5 1 3 5) #u8(1 3 5 1 3 5))
(assert/equal (bytevector) #u8())
(assert/equal (bytevector-length #u8(1 3 5 1 3 5)) 6)
(assert/equal 
    (bytevector-u8-ref #u8(1 1 2 3 5 8 13 21) 5)
    8)

; TODO: compiler support
(assert/equal 
    (let ((bv (bytevector 1 2 3 4)))
        (bytevector-u8-set! bv 1 3)
        bv)
    #u8(1 3 3 4))

(let ()
    (define a #u8(1 2 3 4 5))
    (assert/equal
        (bytevector-copy a 2 4)
        #u8(3 4)))

; TODO: bytevector-copy! (a special form? stdlib function?)
;(define a (bytevector 1 2 3 4 5))
;(define b (bytevector 10 20 30 40 50))
;(bytevector-copy! b 1 a 0 2)
;b
;=⇒ #u8(10 1 2 40 50)

(assert/equal
    (bytevector-append #u8(0 1 2) #u8(3 4 5))
    #u8(0 1 2 3 4 5))

(assert/equal
    (utf8->string #u8(#x41))
    "A")
(assert/equal
    (string->utf8 "λ")
    #u8(#xCE #xBB))

(unit-test-handler-results)
