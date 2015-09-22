#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4
#

MMC_MAKE=mmc --make
CC=gcc

# Debugging
MCFLAGS=--use-grade-subdirs --grade asm_fast.gc.decldebug
CFLAGS=-std=c11 -D_POSIX_C_SOURCE=2 -Wall -Werror -g

# Optimisation
#MCFLAGS=--use-grade-subdirs -O4 --intermodule-optimisation
#CFLAGS=-std=c11 -D_POSIX_C_SOURCE=2 -Wall -O3

vpath %.m src
vpath %.c runtime
vpath %.h runtime
vpath %.o runtime

MERCURY_SOURCES=$(wildcard src/*.m)
C_SOURCES=runtime/pz_main.c \
		runtime/pz.c \
		runtime/pz_code.c \
		runtime/pz_data.c \
		runtime/pz_instructions.c \
		runtime/pz_read.c \
		runtime/pz_run_generic.c \
		runtime/io_utils.c
C_HEADERS=$(wildcard runtime/*.h)
C_OBJECTS=$(patsubst %.c,%.o,$(C_SOURCES))

DOCS_HTML=docs/index.html \
	docs/styleguides/C_style.html \
	docs/styleguides/Mercury_style.html

TEST_DIFFS= \
	examples/pzt/hello.diff \
	examples/pzt/temperature.diff

.PHONY: all
all : tags src/pzasm runtime/pzrun docs

src/pzasm : $(MERCURY_SOURCES)
	(cd src; $(MMC_MAKE) $(MCFLAGS) pzasm)
	(cd src; touch pzasm)
src/pz.bytecode.m: pz_format.h pz_instructions.h
	touch $@
src/pz.code.m: pz_instructions.h
	touch $@

runtime/pzrun : $(C_OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^

%.o : %.c $(C_HEADERS)
	$(CC) $(CFLAGS) -o $@ -c $<

.PHONY: test
test : $(TEST_DIFFS)

%.pz : %.pzt src/pzasm
	./src/pzasm $<

%.diff : %.out %.exp
	diff -u $^ > $@

%.out : %.pz runtime/pzrun
	runtime/pzrun $< > $@

.PHONY: tags
tags : src/tags runtime/tags
src/tags : $(MERCURY_SOURCES)
	(cd src; mtags *.m)
runtime/tags: $(C_SOURCES) $(C_HEADERS)
	(cd runtime; ctags *.c *.h)

.PHONY: docs
docs : $(DOCS_HTML)

%.html : %.txt
	asciidoc $<

.PHONY: clean
clean :
	rm -rf src/Mercury src/tags src/pzasm src/*.err src/*.mh
	rm -rf runtime/tags runtime/pzrun runtime/*.o
	rm -rf $(DOCS_HTML)
	rm -rf examples/pzt/*.pz examples/pzt/*.diff examples/pzt/*.out

