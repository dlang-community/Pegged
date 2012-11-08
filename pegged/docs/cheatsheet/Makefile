# A (very) simple build script to generate a PDF for a LaTeX project.

SRC      := cheatsheet
BUILDDIR := build

LATEXC      := pdflatex
LATEXCFLAGS := -interaction=nonstopmode -output-directory ${BUILDDIR}

.PHONY: all clean
all::
	-@mkdir ${BUILDDIR} 2>/dev/null; exit 0
	@${LATEXC} ${LATEXCFLAGS} ${SRC}

clean::
	-@rm -rf ${SRCDIR}/*~ ${BUILDDIR}
