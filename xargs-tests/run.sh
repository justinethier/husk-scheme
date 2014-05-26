#!/bin/bash

huskc --nolibs test2.scm
./test2
huskc --nolibs test3.scm
./test3
huskc --nolibs test-if.scm
./test-if
huskc --nolibs test-let.scm
./test-let
huskc --nolibs test.scm
./test
huskc --nolibs test-set.scm
./test-set

