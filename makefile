all:
	if [ -x /usr/bin/dmd ]; then \
		dmd -w -wi -O -release -noboundscheck -lib -oflibpegged.a pegged/peg.d pegged/grammar.d pegged/parser.d pegged/introspection.d pegged/dynamic/grammar.d pegged/dynamic/peg.d; \
	elif [ -x /usr/bin/gdc ]; then \
		gdc -c -O -olibpegged.a pegged/peg.d pegged/grammar.d pegged/parser.d pegged/dynamic/grammar.d pegged/dynamic/peg.d; \
	fi

clean:
		rm -f libpegged.a
