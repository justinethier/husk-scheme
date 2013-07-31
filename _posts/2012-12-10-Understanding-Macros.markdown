---
layout: post
title: Understanding Scheme Macros
excerpt: This blog post gives a nice background on the history of various Scheme macro systems.
---
# {{ page.title }}

[This blog post](http://yalfs.blogspot.com/2010/01/understanding-scheme-macros.html) gives a nice background on the history of various Scheme macro systems, and was a big help in implementing husk's macro subsystem.

In general the R<sup>n</sup>RS reports are excellent. But it is difficult to understand all of the `syntax-rules` system from only the language standard. The standard also says nothing about writing an efficient implementation or how to account for hygiene, and there are several non-standard macro systems floating around as well.

Anyway, here are links to the macro systems implemented by husk:

- [Macros that Work](http://www.google.com/search?q=macros+that+work) - The original paper that layed out an efficient implementation of `syntax-rules`. This is the standard Scheme macro system, as specified by R<sup>5</sup>RS and R<sup>7</sup>RS.
- [Hygienic Macros Through Explicit Renaming](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.53.5184) - A low level facility that can be used to break macro hygiene. ER macros also give you to access to the complete Scheme language during macro expansion.

Macros are great, but after working with them for awhile I think you will agree they are both the most advanced part of Scheme and one of the most confusing areas for a beginner - so these resources are essential. 
