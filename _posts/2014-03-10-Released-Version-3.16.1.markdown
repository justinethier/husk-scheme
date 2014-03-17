--- 
layout: post
title: Released Version 3.16.1
excerpt: This release contains library and syntax-rules bug fixes.
---
# {{ page.title }}

- Allow import of a library in the same directory as a program. For example to import `lib.sld`:

        (import (lib))
- Husk no longer throws an error during expansion of a library macro that references another macro which is not exported from the same library.
- Fixed a bug where a `syntax-rules` macro's literal identifier would not match the input when both identifiers are equal and both have no lexical binding.
