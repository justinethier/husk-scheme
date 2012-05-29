(load-ffi "Language.Scheme.Plugins.Json" "jsDecode" "json:decode")
(load-ffi "Language.Scheme.Plugins.Json" "jsEncode" "json:encode")

(write (json:encode '()))
(newline)
(write (json:encode '(test 1 2 3)))
(newline)
