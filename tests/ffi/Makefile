#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4
#

TOP=../..

TOOLS=$(TOP)/src/plzc $(TOP)/src/plzlnk $(TOP)/src/plzbuild

.PHONY: all
all: $(wildcard *.p) BUILD.plz foreign \
		$(TOP)/src/plzbuild $(TOP)/src/plzc $(TOP)/src/plzlnk
	$(TOP)/src/plzbuild --rebuild

.PHONY: foreign
foreign : import_function.so

import_function.pz : all

.PHONY: %.test
%.test : %.exp %.outs
	diff -u $^

.PHONY: %.gctest
%.gctest : %.pz $(TOP)/runtime/plzrun
	PZ_RUNTIME_DEV_OPTS=gc_zealous $(TOP)/runtime/plzrun $< > /dev/null

%.outs : %.out
	grep -v '^#' < $< | sed -e 's/#.*$$//' > $@

%.out : %.pz $(TOP)/runtime/plzrun
	$(TOP)/runtime/plzrun $< > $@

# This target can be used form manual testing, eg to look for differences
# between traces of the same program.
%.trace: %.pz $(TOP)/runtime/plzrun
	 PZ_RUNTIME_DEV_OPTS=interp_trace $(TOP)/runtime/plzrun $< 2>&1 | sed -e 's/0x[0-9a-f]\+/0xXXXX/g' > $@

.PHONY: clean
clean:
	rm -rf *.so *.o
	rm -rf *.pz *.diff *.log *.trace
	rm -rf _build

.PHONY: realclean
realclean: clean
	rm -rf *.plasma-dump_*

# Temporary
%.o : %.c
	gcc -fpic -o $@ -c $<
%.o : %.cpp
	g++ -fpic -o $@ -c $<

import_function.so : import_function.o
	gcc -fpic -shared -o $@ $<

import_function.out : import_function.pz import_function.so \
		$(TOP)/runtime/plzrun
	$(TOP)/runtime/plzrun $< > $@

