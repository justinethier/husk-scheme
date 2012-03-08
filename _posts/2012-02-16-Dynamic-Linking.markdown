---
layout: post
title: Dynamic linking
excerpt: By default, Haskell executables are statically linked... 
---
# {{ page.title }}

By default, Haskell executables are statically linked. Most of the time this is fine, but static linking can become a problem when you go to build your test suite and wind up with an executable like this:

    justin@ubuntu:~$ ./huskc t-basic.scm 
    ...
    justin@ubuntu:~$ ls -l t-basic
    -rwxrwxr-x 1 justin justin 42357805 2012-02-16 14:38 t-basic

That's right, a 42 megabyte executable for a program that just consists of a few unit tests! But it is possible to enable dynamic linking with a bit of effort. The main catch is that we need to recompile each of the packages that husk depends on:

    cabal update
    cabal install mtl transformers utf8-string terminfo text \
                  parsec haskeline --enable-shared --reinstall
    cabal install husk-scheme --enable-shared --reinstall

The key thing to remember is the `--enable-shared` directive which builds a shared library. Fortunately the base packages already come with shared libraries so they do not need to be recompiled.

Anyway, now let's rebuild that test suite. The `-d` flag is a new feature in version 3.5.3 that instructs `huskc` to create a dynamically linked executable:

    justin@ubuntu:~$ huskc -d t-basic.scm 
    ...
    justin@ubuntu:~$ ls -l t-basic
    -rwxrwxr-x 1 justin justin 2935380 2012-02-16 14:39 t-basic

With dynamic linking our compiled executable is now less than 3 megs. And it can be made *even smaller* by stripping out unnecessary debug information:

    justin@ubuntu:~$ strip -p --strip-unneeded --remove-section=.comment -o t-basic-small t-basic
    justin@ubuntu:~$ ls -l
    ... 
    -rwxrwxr-x 1 justin justin 2935380 2012-02-16 14:39 t-basic
    -rwxrwxr-x 1 justin justin 2074536 2012-02-16 14:39 t-basic-small

So we took an executable that was orginally over 42 megs, and managed to strip it down all the way to 2 megs. Not bad!

Now that said, most of the time static linking is good enough, and is convenient since this is how everything works out-of-the-box. But it is good to have a work-around when you need it.
