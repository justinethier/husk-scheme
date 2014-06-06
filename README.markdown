[<img src="https://github.com/justinethier/husk-scheme/raw/master/docs/husk-scheme.png" alt="husk-scheme">](http://justinethier.github.com/husk-scheme)

Husk is a dialect of Scheme written in Haskell that implements a superset of the [R<sup>5</sup>RS standard](http://www.schemers.org/Documents/Standards/R5RS/HTML/) and a large portion of the R<sup>7</sup>RS-small language. Advanced features are provided including continuations, hygienic macros, libraries, and a full numeric tower.

Husk may be used as either a stand-alone interpreter or as an extension language within a larger Haskell application. By closely following the R<sup>5</sup>RS standard, the intent is to develop a Scheme that is as compatible as possible with other R<sup>5</sup>RS Schemes. Husk is mature enough for use in production applications, however it is not optimized for performance-critical applications. 

Scheme is one of two main dialects of Lisp. Scheme follows a minimalist design philosophy: the core language consists of a small number of fundamental forms which may be used to implement other built-in forms. Scheme is an excellent language for writing small, elegant programs, and may also be used to write scripts or embed scripting functionality within a larger application.

More information is available on the [husk website](http://justinethier.github.com/husk-scheme).

Installation
------------
The [Glasgow Haskell Compiler](http://www.haskell.org/ghc/) (GHC) is required to build, install, and run Husk. All recent versions of GHC are supported, including 7.0, 7.2, 7.4, and 7.6. The easiest way to get GHC is via the [Haskell Platform](http://hackage.haskell.org/platform/).

Husk may be installed using [cabal](http://www.haskell.org/cabal/):

    cabal update
    cabal install husk-scheme

Before running Husk you may also need to add the cabal executable directory to your path. On Linux this is `~/.cabal/bin`. Now you are ready to start up the interpreter:

    justin@my-pc$ huski
      _               _        __                 _                          
     | |             | |       \\\               | |                         
     | |__  _   _ ___| | __     \\\      ___  ___| |__   ___ _ __ ___   ___  
     | '_ \| | | / __| |/ /    //\\\    / __|/ __| '_ \ / _ \ '_ ` _ \ / _ \ 
     | | | | |_| \__ \   <    /// \\\   \__ \ (__| | | |  __/ | | | | |  __/ 
     |_| |_|\__,_|___/_|\_\  ///   \\\  |___/\___|_| |_|\___|_| |_| |_|\___| 
                                                                             
     http://justinethier.github.com/husk-scheme                              
     (c) 2010-2012 Justin Ethier                                             
     Version 3.6.2 
                                                                             
    huski> (define (hello) 'world)
    (lambda () ...)
    huski> (hello)
    world

Husk has been tested on Windows, Linux, and FreeBSD.

Documentation
-------------
The online [user manual](http://justinethier.github.io/husk-scheme/manual/index.html) provides an overview of the Scheme language as implemented by Husk, instructions for using the Haskell API, an alphabetical index of Scheme functions, and more.

**Directory Structure**

 - **`docs`** - Documentation has been moved from here to the [`gh-pages` branch](http://justinethier.github.io/husk-scheme/).
 - **`examples`** - Example programs, mostly written in Scheme.
 - **`extensions`** - Haskell-based extensions to Husk.
 - **`hs-src`** - Haskell source code for Husk.
 - **`lib`** - Library portions of Husk written in Scheme.
 - **`scripts`** - Build scripts for Husk and a basic Emacs integration script.
 - **`tests`** - Functional tests for Husk. These can be run automatically by using `make test` from the main Husk directory.


License
-------
Copyright (C) 2010 [Justin Ethier](http://github.com/justinethier)

Husk scheme is available under the [MIT license](http://www.opensource.org/licenses/mit-license.php).

The interpreter is based on code from the book [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) written by Jonathan Tang and hosted / maintained by Wikibooks.

