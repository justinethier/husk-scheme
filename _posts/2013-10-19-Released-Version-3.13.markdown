--- 
layout: post
title: Released Version 3.13
excerpt: This release includes many smaller enhancements to support R<sup>7</sup>RS.
---
# {{ page.title }}

This release includes many enhancements to improve support for R<sup>7</sup>RS:

- Added the command line flag `--revision 7` (or `-r7` for short) to allow huski and huskc to start in R<sup>7</sup>RS mode.
- Added most of the standard R<sup>7</sup>RS libraries: `(scheme base)`, `(scheme char)`, etc.
- Extended syntax-rules to allow another identifier to be used to specify the ellipsis symbol, per R<sup>7</sup>RS. For example, `:::` could be used instead:

        (define-syntax and
          (syntax-rules ::: ()
            ((and test1 test2 :::)
             (if test1 (and test2 :::) #f))))

- Added the following functions from R<sup>7</sup>RS: `make-list` , `list-copy` , `list-set!` , `vector-copy` , `vector-map` , `vector-for-each` , `vector-append` , `string-map` , `string-for-each` , `string->vector` , `vector->string` , `vector-copy!` , `string-copy!` 

