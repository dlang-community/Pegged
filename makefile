all:
	dmd -property -w -wi -O -release -noboundscheck -lib -oflibpegged.a pegged/peg.d pegged/grammar.d pegged/parser.d
clean:
	rm -f libpegged.a
