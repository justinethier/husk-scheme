---
layout: post
title: Released Version 3.5.1 
excerpt: This release contains various incremental improvements to husk...
---
# {{ page.title }}

This release contains various incremental improvements to husk. The major changes are:

- Reduced variable access time by using a Map to store variables within an environment. 
- Improved support for comparing instances of functions using the equality operators `eq?`, `eqv?`, etc.
- Various internal changes such as renaming the tests directory and integrating R<sup>5</sup>RS pitfalls with the unit tests.
