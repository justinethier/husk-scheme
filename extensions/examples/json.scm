; Dynamically load functions from the JSON module
; This takes Haskell a few moments...
(load-ffi "Language.Scheme.Plugins.JSON" "jsDecode" "json:decode")
(load-ffi "Language.Scheme.Plugins.JSON" "jsDecodeStrict" "json:decode-strict")
(load-ffi "Language.Scheme.Plugins.JSON" "jsEncode" "json:encode")
(load-ffi "Language.Scheme.Plugins.JSON" "jsEncodeStrict" "json:encode-strict")

(write (json:encode '()))
(write (json:encode '(test 1 2 3)))
(write (json:encode '#((a 1) (b 2) (c 3) (d (1 2 3 4 #((e 5)))))))
;(json:decode 'blah)

(define address-rec 
 "{
     \"firstName\": \"John\",
     \"lastName\" : \"Smith\",
     \"age\"      : 25,
     \"address\"  :
     {
         \"streetAddress\": \"333 West Camden Street\",
         \"city\"         : \"Baltimore\",
         \"state\"        : \"MD\",
         \"postalCode\"   : \"21201\"
     },
     \"phoneNumber\":
     [
         {
           \"type\"  : \"home\",
           \"number\": \"212 555-1234\"
         },
         {
           \"type\"  : \"fax\",
           \"number\": \"646 555-4567\"
         }
     ]
  }")

(write
    (assq 'city (vector->list (cadr (assq 'address (vector->list (json:decode address-rec)))))))

; Just thinking, since hashtables are more efficient than alists,
; why does scheme have built-in vectors via #() but only clumsy
; library support for hashtables? what would a native hashtable
; syntax look like? or at least, a simple syntax for representing
; hashtables?
;
; #h((a 1) (b 2) (c 3))
;
