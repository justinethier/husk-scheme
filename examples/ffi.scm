(load-ffi "Language.Scheme.Plugins.Examples" "test" "test")
(load-ffi "Language.Scheme.Plugins.Examples" "test2" "test2")
(load-ffi "hs-src/Language/Scheme/Plugins/Examples.hs" "Language.Scheme.Plugins.Examples" "test" "test3")

(write (test 1))
(write (test "1"))
(write (test2 1))
(write (test3 "1"))
