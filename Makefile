skim: parser.hs
	ghc --make -package parsec -fglasgow-exts -o skim parser.hs

# Run all unit tests
test: skim
	cd test-scripts ; ../skim t-case.scm
	cd test-scripts ; ../skim t-cond.scm

clean:
	rm -f *.o
	rm -f *.hi
	rm -f skim
