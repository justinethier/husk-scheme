--- 
layout: post
title: Released Version 3.19
excerpt: Call history is now provided when an error is thrown, to assist debugging.
---
# {{ page.title }}

- Added support for displaying call history when an error is thrown:

        huski> ((lambda () (vector-length "v")))
        Invalid type: expected vector, found "v"

        Call History:
        #0: (vector-length "v")
        #1: ((lambda () (vector-length "v")))

  Sorry this took so long, as it should be a tremendous help for debugging!
- Print the number of received arguments when displaying an "incorrect number of arguments" error message. Also unbox any objects before displaying the error message.
- Allow `read-all` to read from `stdin` when no arguments are received.

Bug Fixes:

- Fixed bugs in `open-input-string` and `open-byte-vector` that prevented a variable being passed as the "input" argument. Thanks to Dan Cecile for the bug report!
- Fixed a bug that could cause an infinite loop during macro expansion. Thanks to Dan Cecile for this report as well.
- Return the empty string from `string-append` if no arguments are received, instead of throwing an error.
- Throw an error when a function that takes a variable number of arguments is called without the minimum number of required arguments. For example `(map list)` should throw an error because at least 2 arguments are required.
- Use `System.Process` instead of deprecated `System.Cmd`.
