; Dynamically load functions from the JSON module
; This takes Haskell a few moments...
(load-ffi "Language.Scheme.Plugins.JSON" "jsDecode" "json:decode")
(load-ffi "Language.Scheme.Plugins.JSON" "jsDecodeStrict" "json:decode-strict")
(load-ffi "Language.Scheme.Plugins.JSON" "jsEncode" "json:encode")
(load-ffi "Language.Scheme.Plugins.JSON" "jsEncodeStrict" "json:encode-strict")

(write (json:encode '()))
(write (json:encode '(test 1 2 3)))
(write (json:encode '#((a 1) (b 2) (c 3) (d (1 2 3 4 #((e 5)))))))
(json:decode 'blah)
