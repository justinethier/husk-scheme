simple_parser: parser.hs
	ghc --make -package parsec -o simple_parser parser.hs

clean:
	rm -f *.o
	rm -f *.hi
	rm -f simple_parser
