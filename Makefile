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
		runtime/pz_library.cpp \
		runtime/pz_option.cpp \
		runtime/pz_read.cpp \
		runtime/pz_generic.cpp \
		runtime/pz_generic_builder.cpp

C_CXX_SOURCES=$(C_SOURCES) $(CXX_SOURCES)
C_HEADERS=$(wildcard runtime/*.h)
OBJECTS=$(patsubst %.c,%.o,$(C_SOURCES)) $(patsubst %.cpp,%.o,$(CXX_SOURCES))

DOCS_HTML=docs/index.html \
	docs/getting_started.html \
	docs/plasma_ref.html \
	docs/contributing.html \
	docs/dev_howto_make_pr.html \
	docs/dev_compiler_internals.html \
	docs/dev_style_mercury.html \
	docs/dev_style_c.html \
	docs/dev_mercury_grades.html \
	docs/dev_maintainers.html \
	docs/dev_bugtracking.html \
	docs/design_principles.html \
	docs/design_concept_map.html \
	docs/design_types.html \
	docs/design_ideas.html \
	docs/references.html \
	docs/pz_machine.html

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
progs : \
	runtime/plzrun \
	src/plzasm \
	src/plzbuild \
	src/plzc \
	src/plzdisasm \
	src/plzlnk

.PHONY: install
install : progs
	$(INSTALL_DIR) $(DEST_DIR)$(BINDIR)
	$(INSTALL) runtime/plzrun $(DEST_DIR)$(BINDIR)
	$(INSTALL) src/plzasm $(DEST_DIR)$(BINDIR)
	$(INSTALL) src/plzbuild $(DEST_DIR)$(BINDIR)
	$(INSTALL) src/plzc $(DEST_DIR)$(BINDIR)
	$(INSTALL) src/plzdisasm $(DEST_DIR)$(BINDIR)
	$(INSTALL) src/plzlnk $(DEST_DIR)$(BINDIR)

# .mer_progs must be real and not a phony target to make this work with
# make -j
src/plzasm : .mer_progs
	touch src/plzasm
src/plzbuild : .mer_progs
	touch src/plzbuild
src/plzc : .mer_progs
	touch src/plzc
src/plzdisasm : .mer_progs
	touch src/plzdisasm
src/plzlnk : .mer_progs 
	touch src/plzlnk
.mer_progs : $(MERCURY_SOURCES)
	rm -f src/*.err
	(cd src; $(MMC_MAKE) $(MCFLAGS) plzasm)
	(cd src; $(MMC_MAKE) $(MCFLAGS) plzbuild)
	(cd src; $(MMC_MAKE) $(MCFLAGS) plzc)
	(cd src; $(MMC_MAKE) $(MCFLAGS) plzdisasm)
	(cd src; $(MMC_MAKE) $(MCFLAGS) plzlnk)
	touch .mer_progs

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
test : src/plzasm src/plzlnk src/plzc src/plzbuild runtime/plzrun
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
	rm -f src/tags 
	rm -f src/plzasm src/plzbuild src/plzc src/plzdisasm src/plzlnk
	rm -rf src/Mercury
	rm -f .mer_progs
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

include $(wildcard $(patsubst %,$(DEPDIR)/%.d,$(basename $(C_CXX_SOURCES))))

