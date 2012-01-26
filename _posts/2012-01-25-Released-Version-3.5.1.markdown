---
layout: post
title: Released Version 3.4.4 
excerpt: This release continues the trend of quick point releases for the 3.4.x series...
---
# {{ page.title }}

This release contains various incremental improvements to husk. The major changes are:

- Reduced variable access time by using a Map to store variables within an environment. 
- Improved support for comparing instances of functions using the equality operators `eq?`, `eqv?`, etc.
- Various internal changes such as renaming the tests directory and integrating R<sup>5</sup>RS pitfalls with the unit tests.
