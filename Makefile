simple_parser: parser.hs
	ghc -package parsec -o simple_parser parser.hs

clean:
	rm -f *.o
	rm -f *.hi
	rm -f simple_parser
