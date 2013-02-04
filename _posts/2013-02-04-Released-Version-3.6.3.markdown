---
layout: post
title: Released Version 3.6.3
excerpt: This release adds R<sup>7</sup>RS bytevector support and makes it easier to use husk as an extension language.
---
# {{ page.title }}

Added support for R<sup>7</sup>RS bytevectors.

Added the Haskell function `evalLisp'` to evaluate a lisp data structure and return the `LispVal` or `LispError` result directly:

    evalLisp' :: Env -> LispVal -> IO (ThrowsError LispVal)

This makes it much easier to retrieve results when using husk as an extension language:

    result <- evalLisp' env $ List [Atom "/", Number 1, Number 0]
    case result of
      Left err -> putStrLn $ "Error: " ++ (show err)
      Right val -> putStrLn $ show val

Fixed a bug where setting a variable to refer back to itself would result in an infinite loop. For example, the last line of the following code would cause `huski` to hang:

    (define a '())
    (define b a)
    (define a b)
