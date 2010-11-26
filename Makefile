# TODO: see http://en.wikibooks.org/wiki/Haskell/Packaging
#       for a proper haskell way to set this up.
husk: hs-src/shell.hs hs-src/Scheme/Core.hs hs-src/Scheme/Macro.hs hs-src/Scheme/Numerical.hs hs-src/Scheme/Parser.hs hs-src/Scheme/Types.hs hs-src/Scheme/Variables.hs
	ghc --make -package parsec -fglasgow-exts -o huski hs-src/shell.hs hs-src/Scheme/Core.hs hs-src/Scheme/Macro.hs hs-src/Scheme/Numerical.hs hs-src/Scheme/Parser.hs hs-src/Scheme/Types.hs hs-src/Scheme/Variables.hs

#husk-lib: core.hs macro.hs numerical.hs parser.hs types.hs variables.hs
#	ghc --make -package parsec -fglasgow-exts -o huski shell.hs core.hs macro.hs numerical.hs parser.hs types.hs variables.hs

cabal:
	runhaskell Setup.hs configure --user
	runhaskell Setup.hs build 
	# TODO: runhaskell Setup.hs install

# Run all unit tests
test:
#	cd scm-unit-tests ; ../huski t-backquote.scm
	cd scm-unit-tests ; ../huski t-case.scm
	cd scm-unit-tests ; ../huski t-cond.scm
	cd scm-unit-tests ; ../huski t-delay.scm
	cd scm-unit-tests ; ../huski t-macro.scm
	cd scm-unit-tests ; ../huski t-numerical-ops.scm
	cd scm-unit-tests ; ../huski t-hashtable.scm
	cd scm-unit-tests ; ../huski t-special-forms.scm
	cd scm-unit-tests ; ../huski t-stdlib.scm
	cd scm-unit-tests ; ../huski t-vector.scm

	cd scm-unit-tests ; ../huski t-iteration.scm

clean:
	rm -f *.o
	rm -f hs-src/*.o
	rm -f hs-src/Scheme/*.o
	rm -f *.hi
	rm -f hs-src/*.hi
	rm -f hs-src/Scheme/*.hi
	rm -f huski
	rm -rf dist
