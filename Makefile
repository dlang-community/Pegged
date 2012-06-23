all:
	dmd -lib -oflibpegged.a pegged/peg.d pegged/grammar.d pegged/utils/associative.d
	dmd -ofpeggeden peggeden.d pegged/peg.d pegged/grammar.d pegged/utils/associative.d
	dmd -ofgen_grammar gen_grammar.d pegged/peg.d pegged/grammar.d pegged/utils/associative.d pegged/examples/PEGGED.d

clean:
	rm -f libpegged.a peggeden gen_grammar
