include setup.mk

all:
	if [ -x /usr/bin/dmd ]; then \
		dmd -w -wi -O -release -noboundscheck -lib -oflibpegged.a $(DFILES); \
	elif [ -x /usr/bin/ldc2 ]; then \
		ldc2 -w -wi -O2 -release -boundscheck=off -c -of=libpegged.a $(DFILES); \
	elif [ -x /usr/bin/gdc ]; then \
		gdc -Wall -O2 -frelease -fbounds-check=off -c -olibpegged.a $(DFILES); \
	fi

clean:
		rm -f libpegged.a

info:
	@echo "DFILES = $(DFILES)"
