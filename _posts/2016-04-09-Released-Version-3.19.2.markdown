--- 
layout: post
title: Released Version 3.19.2
excerpt: Various bug fixes and hash table enhancements
---
# {{ page.title }}

New Features:

- Allow a `default` thunk to be passed to `hash-table-ref`.
- Added `hash-table-ref/default`.

Bug Fixes:

- Fixed `rational?` to properly handle floating-point numbers.
- Migrated `string-fill!` from a macro to a function. This makes it easier to redefine, for example per SRFI 13.
