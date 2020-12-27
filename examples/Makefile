#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4
#

TOP=..

.PHONY: all 
all:
	PATH=$(TOP)/src/$$PATH $(TOP)/src/plzbuild

.PHONY: %.test
%.test : %.exp %.outs
	diff -u $^ 

%.outs : %.out
	grep -v '^#' < $< | sed -e 's/#.*$$//' > $@

.PRECIOUS: %.out
%.out : %.pz all $(TOP)/runtime/plzrun
	$(TOP)/runtime/plzrun $< > $@

.PHONY: clean
clean:
	rm -rf *.pz *.out *.diff *.log _build

.PHONY: realclean
realclean: clean
	rm -rf *.plasma-dump_*

