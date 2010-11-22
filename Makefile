husk: core.hs macro.hs numerical.hs parser.hs types.hs variables.hs
	ghc --make -package parsec -fglasgow-exts -o huski shell.hs core.hs macro.hs numerical.hs parser.hs types.hs variables.hs

# Run all unit tests
test: husk
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
	rm -f *.hi
	rm -f huski
