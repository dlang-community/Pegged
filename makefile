all:
	dmd -property -w -wi -O -release -noboundscheck -lib -oflibpegged.a pegged/peg.d pegged/grammar.d pegged/parser.d pegged/dynamic/grammar.d pegged/dynamic/peg.d

clean:
	rm -f libpegged.a
