---
layout: post
title: Released Version 3.6 
excerpt: Enhanced the variable storage model to improve support for storing references to objects.
---
# {{ page.title }}

Enhanced the variable storage model to improve support for storing references to objects. For example, consider the following:

    (define x (list 'a 'b 'c))
    (define y x)
    (set-cdr! x 4)

After executing this code, previous versions of husk assigned `(a b c)` to `y`. With this release, husk now evaluates `y` to the expected value of `(a . 4)`.

The more general problem is that certain data types denote a memory location which may be modified by mutator functions such as `set-cdr!`. This is discussed specifically in section 3.4 <b>Storage Model</b>:

>
> Variables and objects such as pairs, vectors, and strings implicitly denote locations or sequences of locations. A string, for example, denotes as many locations as there are characters in the string. (These locations need not correspond to a full machine word.) A new value may be stored into one of these locations using the string-set! procedure, but the string continues to denote the same locations as before.
> 

Internally husk uses Haskell data types, so the husk model differs slightly from the one in R<sup>5</sup>RS - references are used instead of individual memory locations. This has implications for the `set-car!` and `set-cdr!` special forms, where circular lists and similar low-level optimizations are not possible as Haskell lists are used instead of raw pointers. However, these issues aside, the enhanced storage model is a big step forward to bringing husk's variable support closer in line to that of other Schemes.

