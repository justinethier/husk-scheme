(load-ffi "Language.Scheme.Plugins.Json" "jsDecode" "json:decode")
(load-ffi "Language.Scheme.Plugins.Json" "jsEncode" "json:encode")

(write (json:encode '()))
(write (json:encode '(test 1 2 3)))
(write (json:encode '#((a 1) (b 2) (c 3) (d (1 2 3 4 #((e 5)))))))
(json:decode 'blah)
