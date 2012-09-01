all:
	dmd -lib -oflibpegged.a pegged/peg.d pegged/grammar.d pegged/parser.d
	dmd regenerate.d pegged/peg.d pegged/grammar.d pegged/parser.d
clean:
	rm -f libpegged.a