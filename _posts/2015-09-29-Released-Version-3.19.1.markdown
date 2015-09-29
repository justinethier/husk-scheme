--- 
layout: post
title: Released Version 3.19.1
excerpt: Bug fixes for complex numbers and GHC 7.10.1.
---
# {{ page.title }}

Bug Fixes:

- Allow `real-part` and `imag-part` to work with all types of numbers. For example, `(imag-part 2.5)` no longer throws an error.
- Applied a fix from Rohan Drape to allow `Compiler` and `Numerical` modules to build in GHC 7.10.1.
