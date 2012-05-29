all:
	dmd -lib -oflibpegged.a pegged/peg.d pegged/grammar.d pegged/utils/associative.d
	dmd -ofpeggeden peggeden.d libpegged.a

clean:
	rm -f libpegged.a peggeden
