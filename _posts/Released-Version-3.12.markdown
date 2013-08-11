--- 
layout: post
title: Released Version 3.12
excerpt: Significant enhancements have been made to the huski REPL in this release.
---
# {{ page.title }}

Significant enhancements have been made to the huski REPL in this release:

- Allow using huski to run Scheme scripts from the shell, as specified by SRFI 22. The script needs to start with the line `#! /usr/bin/env huski` or equivalent, and a `main` function may be defined to receive command line arguments. The `examples/scripts` directory contains example programs `cat.scm` and `sum.scm` that demonstrate how this works in practice.
- Add tab completion for Scheme variables and special forms. Tab completion will still fill in filenames when tab is pressed within double-quotes. This makes it easy to find a file in certain cases such as for a `load`.
- Accept (and ignore) inputs of just whitespace. Previously this would display a nasty error message.

This release also includes the following features:

- Added the `(scheme time)` library from R<sup>7</sup>RS.
- Added the `system` function to make system calls from a husk program. The syntax is `(system "command")`. An integer status code is returned with the same value that the executing program returned to the OS.

Bug fixes:

- Duplicate or otherwise invalid lambda parameters are not allowed, and will throw an error. For example: `(lambda (a a 1) a)`
- It is now a parse error to have a form that includes an empty car cell, for example: `'( . 1)`
- Ensure all of husk's exports are included in the Haskell API documentation.
