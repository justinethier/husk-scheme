---
layout: post
title: Released Version 3.4.4 
excerpt: This release continues the trend of quick point releases for the 3.4.x series...
---
# {{ page.title }}

This release continues the trend of quick point releases for the 3.4.x series. The key change is support for GHC 7.2: 

- husk now compiles in GHC 7.2. This ended up being a simple change - I had mis-interpreted the value of a pre-processor directive. Sorry about that!
- Replaced the definition of `letrec` with the macro from R<sup>5</sup>RS.
- Allow a continuation to call into another continuation via call/cc - see R<sup>5</sup>RS pitfall 1.2.
