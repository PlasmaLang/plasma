#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4
#

#
# Basic configuration
#
JOBS=8
MMC_MAKE=mmc --make -j$(JOBS)
CC=gcc

#
# What kind of build to make
#
# Note that there are also some build parameters in src/Mercury.options
#

# Plain
MCFLAGS=--use-grade-subdirs
CFLAGS=-O2 -std=c99 -D_POSIX_C_SOURCE=200809L -D_BSD_SOURCE -Wall

# Static linking
# MCFLAGS=--use-grade-subdirs --mercury-linkage static
# CFLAGS=-O2 -std=c99 -D_POSIX_C_SOURCE=200809L -Wall

# Dev
# MCFLAGS=--use-grade-subdirs
# CFLAGS=-O1 -std=c99 -D_POSIX_C_SOURCE=200809L -D_BSD_SOURCE -DDEBUG -Wall -Werror -DPZ_DEV

# Dev: Extra checks.
# MCFLAGS+=--warn-dead-procs

# Debugging
# MCFLAGS=--use-grade-subdirs --grade asm_fast.gc.decldebug.stseg
# CFLAGS=-O0 -std=c99 -D_POSIX_C_SOURCE=200809L -D_BSD_SOURCE -DDEBUG -Wall -Werror -g -DPZ_DEV

# Optimisation
# MCFLAGS=--use-grade-subdirs -O4 --intermodule-optimisation
# CFLAGS=-O3 -std=c99 -D_POSIX_C_SOURCE=200809L -D_BSD_SOURCE -Wall

#
# Extra features
#

# Tracing of PZ execution, this will create a lot of output, you were
# warned.
PZ_TRACE=no
# PZ_TRACE=yes

# Tracing of the type checking/inference solver.
# MCFLAGS+=--trace-flag typecheck_solve

# As the build system gets more complex I want to avoid autoconf.  Perhaps
# instead create a config.h and makefile for each major OS+platform
# combination.  An optional configure script could put the right file in
# place.  Also consider autosetup.

#
# No configuration beyond here
#

vpath %.m src
vpath %.c runtime
vpath %.h runtime
vpath %.o runtime
vpath %.txt docs
vpath %.html docs/html

MERCURY_SOURCES=$(wildcard src/*.m)
C_SOURCES=runtime/pz_main.c \
		runtime/pz.c \
		runtime/pz_builtin.c \
		runtime/pz_code.c \
		runtime/pz_data.c \
		runtime/pz_instructions.c \
		runtime/pz_gc.c \
		runtime/pz_radix_tree.c \
		runtime/pz_read.c \
		runtime/pz_run_generic.c \
		runtime/io_utils.c
C_HEADERS=$(wildcard runtime/*.h)
C_OBJECTS=$(patsubst %.c,%.o,$(C_SOURCES))

DOCS_HTML=docs/index.html \
	docs/C_style.html \
	docs/Mercury_style.html \
	docs/concept_map.html \
	docs/grades.html \
	docs/ideas.html \
	docs/plasma_ref.html \
	docs/pz_format.html \
	docs/pz_machine.html \
	docs/references.html \
	docs/types.html

# Extra tracing
ifeq ($(PZ_TRACE),yes)
	CFLAGS+=-DPZ_INSTR_TRACE
	C_SOURCES+=runtime/pz_trace.c
else
endif

.PHONY: all
all : tools runtime/pzrun docs

.PHONY: tools
tools : rm_errs src/pzasm src/plasmac

.PHONY: rm_errs
rm_errs :
	rm -f src/*.err

src/pzasm : $(MERCURY_SOURCES)
	(cd src; $(MMC_MAKE) $(MCFLAGS) pzasm)
	(cd src; touch pzasm)
src/plasmac : $(MERCURY_SOURCES)
	(cd src; $(MMC_MAKE) $(MCFLAGS) plasmac)
	(cd src; touch plasmac)
src/pz.bytecode.m: pz_common.h pz_format.h pz_instructions.h
	touch $@
src/pz.m: pz_common.h pz_format.h
	touch $@

runtime/pzrun : $(C_OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^

%.o : %.c $(C_HEADERS)
	$(CC) $(CFLAGS) -o $@ -c $<

.PHONY: test
test : src/pzasm src/plasmac runtime/pzrun
	(cd tests; ./run_tests.sh)

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

#
# Clean removes all intermediate files
#
.PHONY: clean
clean : localclean
	$(MAKE) -C tests/pzt clean
	$(MAKE) -C tests/valid clean
	$(MAKE) -C tests/invalid clean

#
# Realclean removes all generated files plus plasma-dump files.
#
.PHONY: realclean
realclean : localclean
	$(MAKE) -C tests/pzt realclean
	$(MAKE) -C tests/valid realclean
	$(MAKE) -C tests/invalid realclean
	rm -rf src/tags src/pzasm src/plasmac
	rm -rf src/Mercury
	rm -rf runtime/tags runtime/pzrun
	rm -rf $(DOCS_HTML)

.PHONY: localclean
localclean:
	for dir in \
		date0s \
		date3s \
		dates \
		err_dates \
		int0s \
		int2s \
		int3s \
		ints \
		module_deps ; \
	do \
		rm -rf src/Mercury/$$dir; \
	done
	for dir in cs os c_dates ; do \
		rm -rf src/Mercury/*/*/Mercury/$$dir; \
	done
	rm -rf src/*.err src/*.mh
	rm -rf runtime/*.o
	rm -rf examples/*.pz examples/*.diff examples/*.out

# Nither formatting tool does a perfect job, but clang-format seems to be
# the best.
.PHONY: format
format: formatclangformat

.PHONY: formatclangformat
formatclangformat:
	clang-format -style=file -i $(C_SOURCES) $(C_HEADERS)

# Keep the ident configuration for reference.
.PHONY: formatindent
formatindent:
	indent -i4 -l77 \
		--blank-lines-after-commas \
		--blank-lines-after-procedures \
		--braces-on-if-line \
		--case-brace-indentation 0 \
		--continue-at-parentheses \
		--cuddle-else \
		--declaration-indentation 8 \
		--procnames-start-lines \
		--space-after-if \
		--no-space-after-cast \
		--no-space-after-function-call-names \
		--no-tabs \
		$(C_SOURCES) $(C_HEADERS)

