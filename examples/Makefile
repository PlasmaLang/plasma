#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4
#

TOP=..

.PHONY: all 
all:
	@echo This Makefile does not have an "all" target
	@echo Use the run_tests.sh script in the parent directory to run all tests
	@echo or use "make test_name.test" to run a single test.
	@false

%.pzo : %.p $(TOP)/src/plzc
	$(TOP)/src/plzc $<

%.pz : %.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n ${subst .pz,,$@} -o $@ $<

.PHONY: %.test
%.test : %.exp %.outs
	diff -u $^ 

%.outs : %.out
	grep -v '^#' < $< | sed -e 's/#.*$$//' > $@

.PRECIOUS: %.out
%.out : %.pz $(TOP)/runtime/plzrun
	$(TOP)/runtime/plzrun $< > $@

.PHONY: clean
clean:
	rm -rf *.pz *.pzo *.out *.diff *.log

.PHONY: realclean
realclean: clean
	rm -rf *.plasma-dump_*

