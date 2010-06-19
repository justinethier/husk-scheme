skim: parser.hs
	ghc --make -package parsec -fglasgow-exts -o skim parser.hs

clean:
	rm -f *.o
	rm -f *.hi
	rm -f skim
