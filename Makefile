skim: core.hs macro.hs parser.hs types.hs variables.hs
	ghc --make -package parsec -fglasgow-exts -o skim core.hs macro.hs parser.hs types.hs variables.hs

# Run all unit tests
test: skim
#	cd scm-unit-tests ; ../skim t-backquote.scm
	cd scm-unit-tests ; ../skim t-case.scm
	cd scm-unit-tests ; ../skim t-cond.scm
	cd scm-unit-tests ; ../skim t-delay.scm
	cd scm-unit-tests ; ../skim t-macro.scm
	cd scm-unit-tests ; ../skim t-special-forms.scm
	cd scm-unit-tests ; ../skim t-stdlib.scm
	cd scm-unit-tests ; ../skim t-vector.scm

clean:
	rm -f *.o
	rm -f *.hi
	rm -f skim
