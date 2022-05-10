all:
	if [ -x /usr/bin/dmd ]; then \
		dmd -w -wi -O -release -noboundscheck -lib -oflibpegged.a pegged/peg.d pegged/grammar.d pegged/parser.d pegged/introspection.d pegged/dynamic/grammar.d pegged/dynamic/peg.d; \
	elif [ -x /usr/bin/ldc2 ]; then \
		ldc2 -w -wi -O2 -release -boundscheck=off -c -of=libpegged.a pegged/peg.d pegged/grammar.d pegged/parser.d pegged/dynamic/grammar.d pegged/dynamic/peg.d; \
	elif [ -x /usr/bin/gdc ]; then \
		gdc -Wall -O2 -frelease -fbounds-check=off -c -olibpegged.a pegged/peg.d pegged/grammar.d pegged/parser.d pegged/dynamic/grammar.d pegged/dynamic/peg.d; \
	fi

clean:
		rm -f libpegged.a
