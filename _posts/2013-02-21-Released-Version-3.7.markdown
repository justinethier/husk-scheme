---
layout: post
title: Released Version 3.7
excerpt: This release adds beta support for Scheme libraries as well as many improvements to the Haskell API.
---
# {{ page.title }}

A major change for this release is the introduction of Scheme libraries using R<sup>7</sup>RS library syntax. For an example of how to use libraries, see `examples/hello-library/hello.scm` in the husk source tree. Note that since R<sup>7</sup>RS is not currently implemented by husk, the library system only has the built-in import `(r5rs base)` which allows you to import the standard husk R<sup>5</sup>RS environment. Also, please keep in mind this is still a beta feature that is not yet implemented by the compiler.

This release also contains many improvements to the Haskell API:

- Added `r5rsEnv` to the Core module to expose the full environment, including functions loaded from the Scheme standard library.
- Added `getDataFileFullPath` to the Core module to allow third party code to reference Scheme files such as `stdlib.scm` that are installed alongside husk.
- Modified `NumArgs` to optionally require an explicit number of arguments. This helps when writing variable-length functions where specifying a single number of arguments may be misleading.
- Added a new module `Language.Scheme.Util` to contain general purpose utility functions.

Bug fixes:

- Updated the parser to accept floating point numbers that contain only a fractional component, such as `.5`.
- Enhanced numerical comparison operators (`=`, `<`, `<=`, `>`, `>=`) to be able to accept an unlimited number of arguments, per R<sup>5</sup>RS.
