hs2scm: parser.hs
	ghc --make -package parsec -fglasgow-exts -o hs2scm parser.hs

clean:
	rm -f *.o
	rm -f *.hi
	rm -f hs2scm
