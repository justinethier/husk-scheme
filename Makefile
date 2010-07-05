skim: parser.hs
	ghc --make -package parsec -fglasgow-exts -o skim parser.hs

# Run all unit tests
test: skim
#	cd scm-unit-tests ; ../skim t-backquote.scm
	cd scm-unit-tests ; ../skim t-case.scm
	cd scm-unit-tests ; ../skim t-cond.scm
	cd scm-unit-tests ; ../skim t-special-forms.scm
	cd scm-unit-tests ; ../skim t-stdlib.scm

clean:
	rm -f *.o
	rm -f *.hi
	rm -f skim
