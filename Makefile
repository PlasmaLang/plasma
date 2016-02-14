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
MCFLAGS=--use-grade-subdirs --grade asm_fast.gc.decldebug
CFLAGS=-std=c99 -D_POSIX_C_SOURCE=2 -Wall -Werror -g

# Plain
# MCFLAGS=--use-grade-subdirs --grade asm_fast.gc
# CFLAGS=-std=c99 -D_POSIX_C_SOURCE=2 -Wall -Werror

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

DOCS_HTML=docs/html/index.html \
	docs/html/pz_format.html \
	docs/html/pz_machine.html \
	docs/html/plasma_ref.html \
	docs/html/C_style.html \
	docs/html/Mercury_style.html \
	docs/html/references.html \
	docs/html/todo.html

TEST_DIFFS= \
	examples/pzt/fib.diff \
	examples/pzt/hello.diff \
	examples/pzt/temperature.diff \
	examples/pzt/trunc_ze_se.diff

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
docs : $(DOCS_HTML) docs/images docs/common.css docs/favicon.ico

%.html : %.txt docs/asciidoc.conf
	asciidoc --conf-file docs/asciidoc.conf  -o $@ $<

docs/html/index.html: docs/index.html
	cp $< $@
docs/html/pz_format.html: docs/pz_format.html
	cp $< $@
docs/html/pz_machine.html: docs/pz_machine.html
	cp $< $@
docs/html/plasma_ref.html: docs/plasma_ref.html
	cp $< $@
docs/html/C_style.html: docs/C_style.html
	cp $< $@
docs/html/Mercury_style.html: docs/Mercury_style.html
	cp $< $@
docs/html/references.html: docs/references.html
	cp $< $@
docs/html/todo.html: docs/todo.html
	cp $< $@

docs/images:
	-cp -r ../plasma-website/images $@
docs/common.css:
	-cp ../plasma-website/common.css $@
docs/favicon.ico:
	-cp ../plasma-website/favicon.ico $@

.PHONY: clean
clean :
	rm -rf src/Mercury src/tags src/pzasm src/plasmac src/*.err src/*.mh
	rm -rf runtime/tags runtime/pzrun runtime/*.o
	rm -rf docs/*.html $(DOCS_HTML) docs/images docs/plasma.css \
		docs/favicon.ico
	rm -rf examples/pzt/*.pz examples/pzt/*.diff examples/pzt/*.out

