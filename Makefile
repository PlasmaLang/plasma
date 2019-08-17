#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4
#

# Basic configuration
# ===================
# 
# You can configure the build by adjusting these settings and optionally
# commenting in/out several options below.
#

# How the Mercury compiler should be called.  You may need to adjust this if
# it is not in your path.
MMC_MAKE=mmc --make -j$(JOBS)

# The number of parallel jobs the Mercury compiler should spawn.
JOBS=8

# How the C compiler should be called.  gcc and clang should both work.
# Note that Mercury has its own configuration for its C backend, which is
# not, and must not be changed here.
CC=gcc

# gcc and probably clang support dependency tracking.  If your compiler
# doesn't uncomment the 2nd line.
DEPDIR=.dep
DEPFLAGS=-MT $@ -MMD -MP -MF $(DEPDIR)/$(basename $*).Td

# How to call asciidoc (optional). A full path or any flags here won't work
# without other changes to the makefile.
ASCIIDOC=asciidoc

# How to call clang-format (optional)
CLANGFORMAT=clang-format

# How to call indent (optional)
INDENT=indent

# Detailed build options
# ----------------------
#
# The following settings are closely related and therefore we provide
# suggestions in groups, depending on what type of build you want.
#
# Note that there are also some build parameters in src/Mercury.options

# We start with a base configuration, these work on Linux but you may need
# to change them for other systems / compilers.
MCFLAGS=--use-grade-subdirs
C_CXX_FLAGS=-O1 -D_POSIX_C_SOURCE=200809L -D_DEFAULT_SOURCE
C_CXX_WARN_FLAGS=-Wall -Wno-error=pointer-arith -Wno-pointer-arith
C_ONLY_FLAGS=-std=c99
CXX_ONLY_FLAGS=-std=c++11 -fno-rtti -fno-exceptions

# This is a suitable build for development.  It has assertions enabled in
# the C code some of which are slow, so they shouldn't be used for
# performance measurement.  Comment it out to use one of the optimised
# builds below.
#
# Note to maintainers: When Plasma is actually "used" we should change this
# default and provide a better way for developers to setup a "dev" build
# with assertions and other checks.

# Development build options
MCFLAGS+=--warn-dead-procs
C_CXX_WARN_FLAGS+=-Werror -DDEBUG -DPZ_DEV
PZ_DEV=yes

# You can uncomment _at most one_ of the following sets of options, or write
# your own.

# Enable C and Mercury debugging.
# MCFLAGS=--use-grade-subdirs --grade asm_fast.gc.decldebug.stseg
# C_CXX_FLAGS=-O0 -D_POSIX_C_SOURCE=200809L -D_DEFAULT_SOURCE -DDEBUG -g -DPZ_DEV

# Enable static linking
# MCFLAGS=--use-grade-subdirs --mercury-linkage static
# C_CXX_FLAGS=-O2 -D_POSIX_C_SOURCE=200809L -D_DEFAULT_SOURCE -Wno-error

# Enable optimisation,
# Remember to comment-out the development build options above.
# MCFLAGS=--use-grade-subdirs -O4 --intermodule-optimisation
# C_CXX_FLAGS=-O3 -D_POSIX_C_SOURCE=200809L -D_DEFAULT_SOURCE -Wno-error

# Enable both static linking and optimisation
# Remember to comment-out the development build options above.
# MCFLAGS=--use-grade-subdirs -O4 --intermodule-optimisation \
#   --mercury-linkage static
# C_CXX_FLAGS=-O3 -D_POSIX_C_SOURCE=200809L -D_DEFAULT_SOURCE -Wno-error

# Extra features
# --------------
#
# These can be uncommented to add extra features of interest to developers.

# Tracing of the type checking/inference solver.
# MCFLAGS+=--trace-flag typecheck_solve

# No configuration beyond here
# ============================

# As the build system gets more complex I want to avoid autoconf.  Perhaps
# instead create a config.h and makefile for each major OS+platform
# combination.  An optional configure script could put the right file in
# place.  Also consider autosetup.

vpath %.m src
vpath %.c runtime
vpath %.cpp runtime
vpath %.h runtime
vpath %.o runtime
vpath %.txt docs
vpath %.html docs/html

MERCURY_SOURCES=$(wildcard src/*.m)

# There are no C sources but we keep this in case we add some C code (eg
# a library interface.)  The tags target will need to be fixed if C sources
# are added.
C_SOURCES=

# NOTE that when we add alternative interpreters we'll need to seperate out
# the generic files, that includes updating pz_closure.h so it includes
# different files.
CXX_SOURCES=runtime/pz_main.cpp \
		runtime/pz.cpp \
		runtime/pz_builtin.cpp \
		runtime/pz_code.cpp \
		runtime/pz_cxx_future.cpp \
		runtime/pz_data.cpp \
		runtime/pz_generic_closure.cpp \
		runtime/pz_generic_builtin.cpp \
		runtime/pz_generic_run.cpp \
		runtime/pz_gc.cpp \
		runtime/pz_gc_alloc.cpp \
		runtime/pz_gc_collect.cpp \
		runtime/pz_gc_util.cpp \
		runtime/pz_instructions.cpp \
		runtime/pz_io.cpp \
		runtime/pz_module.cpp \
		runtime/pz_option.cpp \
		runtime/pz_read.cpp \
		runtime/pz_generic.cpp \
		runtime/pz_generic_builder.cpp

C_CXX_SOURCES=$(C_SOURCES) $(CXX_SOURCES)
C_HEADERS=$(wildcard runtime/*.h)
OBJECTS=$(patsubst %.c,%.o,$(C_SOURCES)) $(patsubst %.cpp,%.o,$(CXX_SOURCES))

DOCS_HTML=docs/index.html \
	docs/C_style.html \
	docs/Mercury_style.html \
	docs/bugtracking.html \
	docs/concept_map.html \
	docs/design_principles.html \
	docs/grades.html \
	docs/howto_make_pr.html \
	docs/getting_started.html \
	docs/ideas.html \
	docs/plasma_ref.html \
	docs/pz_format.html \
	docs/pz_machine.html \
	docs/references.html \
	docs/types.html

# Extra development modules
ifeq ($(PZ_DEV),yes)
	CXX_SOURCES+=runtime/pz_trace.cpp
else
endif

ifneq ($(shell which $(ASCIIDOC)),)
	DOCS_TARGETS=$(DOCS_HTML)
else
	DOCS_TARGETS=.docs_warning
endif

CFLAGS=$(DEPFLAGS) $(C_CXX_WARN_FLAGS) $(C_CXX_FLAGS) $(C_ONLY_FLAGS)
CXXFLAGS=$(DEPFLAGS) $(C_CXX_WARN_FLAGS) $(C_CXX_FLAGS) $(CXX_ONLY_FLAGS)
$(shell mkdir -p $(DEPDIR)/runtime >/dev/null)

.PHONY: all
all : tools runtime/plzrun docs

.PHONY: tools
tools : rm_errs src/plzasm src/plzc

.PHONY: rm_errs
rm_errs :
	rm -f src/*.err

src/plzasm : $(MERCURY_SOURCES)
	(cd src; $(MMC_MAKE) $(MCFLAGS) plzasm)
	(cd src; touch plzasm)
src/plzc : $(MERCURY_SOURCES)
	(cd src; $(MMC_MAKE) $(MCFLAGS) plzc)
	(cd src; touch plzc)

# Work around Mercury bug https://bugs.mercurylang.org/view.php?id=472
src/pz.bytecode.m src/pz.bytecode.mh: pz_common.h pz_format.h pz_instructions.h
	touch src/pz.bytecode.m
	test -e src/pz.bytecode.mh && touch src/pz.bytecode.mh || true
src/pz.m src/pz.mh: pz_common.h pz_format.h
	touch $@
	test -e src/pz.mh && touch src/pz.mh || true

runtime/plzrun : $(OBJECTS)
	$(CXX) $(CFLAGS) -o $@ $^

%.o : %.c
	$(CC) $(CFLAGS) -o $@ -c $<
	mv -f $(DEPDIR)/$(basename $*).Td $(DEPDIR)/$(basename $*).d

%.o : %.cpp
	$(CXX) $(CXXFLAGS) -o $@ -c $<
	mv -f $(DEPDIR)/$(basename $*).Td $(DEPDIR)/$(basename $*).d

$(DEPDIR)/%.d : ;
.PRECIOUS: $(DEPDIR)/%.d

.PHONY: test
test : src/plzasm src/plzc runtime/plzrun
	(cd tests; ./run_tests.sh)

.PHONY: tags
tags : src/tags runtime/tags
src/tags : $(MERCURY_SOURCES)
	(cd src; mtags *.m)
runtime/tags: $(CXX_SOURCES) $(C_HEADERS)
	(cd runtime; ctags *.cpp *.h)

.PHONY: docs
docs : $(DOCS_TARGETS)

.docs_warning :
	@echo
	@echo Warning: $(ASCIIDOC) not found, not building documentation.
	@echo --------------------------------------------------------
	@echo
	touch .docs_warning

%.html : %.txt docs/asciidoc.conf
	$(ASCIIDOC) --conf-file docs/asciidoc.conf  -o $@ $<

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
	rm -rf src/tags src/plzasm src/plzc
	rm -rf src/Mercury
	rm -rf runtime/tags runtime/plzrun
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
	rm -rf .docs_warning
	rm -rf $(DEPDIR)

# Nither formatting tool does a perfect job, but clang-format seems to be
# the best.
.PHONY: format
format: formatclangformat

.PHONY: formatclangformat
formatclangformat:
	$(CLANGFORMAT) -style=file -i $(C_SOURCES) $(CXX_SOURCES) $(C_HEADERS)

# Keep the ident configuration for reference.
.PHONY: formatindent
formatindent:
	$(INDENT) -i4 -l77 \
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
		$(C_SOURCES) $(CXX_SOURCES) $(C_HEADERS)

include $(wildcard $(patsubst %,$(DEPDIR)/%.d,$(basename $(C_CXX_SOURCES))))

