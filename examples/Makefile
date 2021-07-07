#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4
#

TOP=..

.PHONY: all
all: $(wildcard *.p) $(TOP)/src/plzbuild $(TOP)/src/plzc $(TOP)/src/plzlnk
	$(TOP)/src/plzbuild

hello.pz: all
fib.pz: all
modules.pz: all
mr4.pz: all
temperature.pz: all
readline.pz: all

.PHONY: %.test
%.test : %.exp %.outs
	diff -u $^

%.outs : %.out
	grep -v '^#' < $< | sed -e 's/#.*$$//' > $@

%.out : %.pz $(TOP)/runtime/plzrun
	$(TOP)/runtime/plzrun $< > $@

readline.out : readline.pz $(TOP)/runtime/plzrun
	echo Paul Bone | $(TOP)/runtime/plzrun $< > $@

.PHONY: clean
clean:
	rm -rf *.pz *.out *.diff *.log _build

.PHONY: realclean
realclean: clean
	rm -rf *.plasma-dump_*

