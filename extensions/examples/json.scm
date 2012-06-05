;;; 
;;; husk-scheme
;;; http://github.com/justinethier/husk-scheme
;;;
;;; Written by Justin Ethier
;;;
;;; An example of how to use the JSON module
;;;

; Dynamically load functions from the JSON module
; This takes Haskell a few moments...
(load-ffi "Language.Scheme.Plugins.JSON" "jsDecode" "json:decode")
(load-ffi "Language.Scheme.Plugins.JSON" "jsDecodeStrict" "json:decode-strict")
(load-ffi "Language.Scheme.Plugins.JSON" "jsEncode" "json:encode")
(load-ffi "Language.Scheme.Plugins.JSON" "jsEncodeStrict" "json:encode-strict")

; Convert s-expressions to JSON
(write (json:encode '()))
(write (json:encode '(test 1 2 3)))
(write (json:encode '#((a 1) (b 2) (c 3) (d (1 2 3 4 #((e 5)))))))

; A JSON record...
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

; Look up city from the JSON record
(write
    (assq 'city (vector->list (cadr (assq 'address (vector->list (json:decode address-rec)))))))

; Or, make it easier by loading address into a hash table
(define ht-address 
    (alist->hash-table 
        (vector->list
            (json:decode-strict address-rec))))
(write (hash-table-ref ht-address 'firstName))
(write (hash-table-ref ht-address 'lastName))
(write (hash-table-ref ht-address 'age))
