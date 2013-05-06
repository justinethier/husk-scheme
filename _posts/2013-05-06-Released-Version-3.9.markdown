--- 
layout: post
title: Released Version 3.9
excerpt: This release resolves a major issue with the FFI module, and includes several smaller fixes.
---
# {{ page.title }}

- Fixed a syntax error that prevented the FFI module from building. Thanks to Ricardo Lanziano for bringing this to my attention!
- Enhanced the compiler to use the name of the source file for the compiled Haskell output instead of hardcoding the file to `_tmp.hs`. For example, `my-file.scm` will compile to `my-file.hs`.
- Removed extraneous quotes when printing a lambda form.
