all:
	dmd -lib -oflibpegged.a pegged/peg.d pegged/grammar.d pegged/parser.d pegged/testerparser.d pegged/grammartester.d pegged/introspection.d pegged/examples/arithmetic.d
	dmd regenerate.d pegged/peg.d pegged/grammar.d pegged/parser.d pegged/introspection.d pegged/examples/peggedgrammar.d pegged/examples/testergrammar.d
clean:
	rm -f libpegged.a regenerate.o
