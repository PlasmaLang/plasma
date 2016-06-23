#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4
#

JOBS=8
MMC_MAKE=mmc --make -j$(JOBS)
CC=gcc

# Debugging
# MCFLAGS=--use-grade-subdirs --grade asm_fast.gc.decldebug.stseg
# CFLAGS=-std=c99 -D_POSIX_C_SOURCE=2 -Wall -Werror -g

# Plain
MCFLAGS=--use-grade-subdirs
CFLAGS=-std=c99 -D_POSIX_C_SOURCE=2 -Wall -Werror

# Optimisation
# MCFLAGS=--use-grade-subdirs -O4 --intermodule-optimisation
# CFLAGS=-std=c99 -D_POSIX_C_SOURCE=2 -Wall -Werror -O3

vpath %.m src
vpath %.c runtime
vpath %.h runtime
vpath %.o runtime
vpath %.txt docs
vpath %.html docs/html

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
	docs/pz_format.html \
	docs/pz_machine.html \
	docs/plasma_ref.html \
	docs/C_style.html \
	docs/Mercury_style.html \
	docs/references.html \
	docs/todo.html

TEST_DIFFS= \
	tests/pzt/fib.diff \
	tests/pzt/hello.diff \
	tests/pzt/mutual.diff \
	tests/pzt/stack.diff \
	tests/pzt/temperature.diff \
	tests/pzt/trunc_ze_se.diff \
	tests/p/operators.diff \
	examples/hello.diff \
	examples/types.diff \
	examples/temperature.diff

.PHONY: all
all : tags src/pzasm src/plasmac runtime/pzrun docs

src/pzasm : $(MERCURY_SOURCES)
	@(cd src; $(MMC_MAKE) $(MCFLAGS) pzasm)
	(cd src; touch pzasm)
src/plasmac : $(MERCURY_SOURCES)
	@(cd src; $(MMC_MAKE) $(MCFLAGS) plasmac)
	(cd src; touch plasmac)
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

%.pz : %.p src/plasmac
	./src/plasmac $<

%.diff : %.exp %.out
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

%.html : %.txt docs/asciidoc.conf
	asciidoc --conf-file docs/asciidoc.conf  -o $@ $<

.PHONY: clean
clean :
	rm -rf src/Mercury src/tags src/pzasm src/plasmac src/*.err src/*.mh
	rm -rf runtime/tags runtime/pzrun runtime/*.o
	rm -rf $(DOCS_HTML)
	rm -rf tests/pzt/*.pz tests/pzt/*.diff tests/pzt/*.out
	rm -rf tests/p/*.pz tests/p/*.diff tests/p/*.out
	rm -rf examples/*.pz examples/*.diff examples/*.out

