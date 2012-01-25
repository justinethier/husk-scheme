#!/bin/bash
./huskc scm-unit-tests/compiler/t-basic.scm
ghc -cpp --make -package ghc -fglasgow-exts -o test _tmp.hs hs-src/Language/Scheme/Primitives.hs hs-src/Language/Scheme/Parser.hs hs-src/Language/Scheme/Numerical.hs hs-src/Language/Scheme/Core.hs hs-src/Language/Scheme/Macro.hs hs-src/Language/Scheme/FFI.hs hs-src/Language/Scheme/Macro/Matches.hs
./test
