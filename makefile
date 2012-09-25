all:
	dmd -lib -oflibpegged.a pegged/peg.d pegged/grammar.d pegged/parser.d pegged/introspection.d
	dmd regenerate.d pegged/peg.d pegged/grammar.d pegged/parser.d pegged/introspection.d pegged/examples/peggedgrammar.d
clean:
	rm -f libpegged.a regenerate.o