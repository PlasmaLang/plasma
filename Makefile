#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4
#

# ======================
# 
# No configuration here
# See build.mk
include defaults.mk
-include build.mk
#
# ======================

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
	docs/compiler_internals.html \
	docs/concept_map.html \
	docs/contributing.html \
	docs/maintainers.html \
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
ifeq ($(BUILD_TYPE),dev)
	CXX_SOURCES+= \
		runtime/pz_gc_debug.cpp \
		runtime/pz_trace.cpp
else
endif

ifneq ($(shell which $(ASCIIDOC)),)
	DOCS_TARGETS=$(DOCS_HTML)
else
	DOCS_TARGETS=.docs_warning
endif

CFLAGS=$(DEPFLAGS) $(C_CXX_FLAGS) $(C_ONLY_FLAGS)
CXXFLAGS=$(DEPFLAGS) $(C_CXX_FLAGS) $(CXX_ONLY_FLAGS)
$(shell mkdir -p $(DEPDIR)/runtime >/dev/null)

.PHONY: all
all : progs docs

.PHONY: progs
progs : rm_errs src/plzasm src/plzlnk src/plzc src/plzdisasm runtime/plzrun

.PHONY: rm_errs
rm_errs :
	rm -f src/*.err

src/plzasm : $(MERCURY_SOURCES)
	(cd src; $(MMC_MAKE) $(MCFLAGS) plzasm)
	(cd src; touch plzasm)
src/plzlnk : $(MERCURY_SOURCES)
	(cd src; $(MMC_MAKE) $(MCFLAGS) plzlnk)
	(cd src; touch plzlnk)
src/plzc : $(MERCURY_SOURCES)
	(cd src; $(MMC_MAKE) $(MCFLAGS) plzc)
	(cd src; touch plzc)
src/plzdisasm : $(MERCURY_SOURCES)
	(cd src; $(MMC_MAKE) $(MCFLAGS) plzdisasm)
	(cd src; touch plzdisasm)

# Work around Mercury bug https://bugs.mercurylang.org/view.php?id=472
src/pz.bytecode.m src/pz.bytecode.mh: pz_common.h pz_instructions.h
	touch src/pz.bytecode.m
	test -e src/pz.bytecode.mh && touch src/pz.bytecode.mh || true
src/pz.format.m src/pz.format.mh: pz_common.h pz_format.h
	touch src/pz.format.m
	test -e src/pz.format.mh && touch src/pz.format.mh || true
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
test : src/plzasm src/plzlnk src/plzc runtime/plzrun
	(cd tests; ./run_tests.sh $(BUILD_TYPE))

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
	$(MAKE) -C examples clean
	$(MAKE) -C tests/pzt clean
	$(MAKE) -C tests/valid clean
	$(MAKE) -C tests/invalid clean
	$(MAKE) -C tests/modules clean
	$(MAKE) -C tests/modules-invalid clean
	$(MAKE) -C tests/missing clean

#
# Realclean removes all generated files plus plasma-dump files.
#
.PHONY: realclean
realclean : localclean
	$(MAKE) -C examples realclean
	$(MAKE) -C tests/pzt realclean
	$(MAKE) -C tests/valid realclean
	$(MAKE) -C tests/invalid realclean
	$(MAKE) -C tests/modules realclean
	$(MAKE) -C tests/modules-invalid realclean
	$(MAKE) -C tests/missing realclean
	rm -rf src/tags src/plzasm src/plzc src/plzlnk src/plzdisasm
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

