#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4
#

TOP=../..

.PHONY: all 
all:
	@echo This Makefile does not have an "all" target
	@echo Use the run_tests.sh script in the parent directory to run all tests
	@echo or use "make test_name.test" to run a single test.
	@false

%.po : %.pzt $(TOP)/src/plzasm
	$(TOP)/src/plzasm $<

%.pz : %.po
	cp $< $@

.PHONY: %.test
%.test : %.exp %.out
	diff -u $^ 

.PHONY: %.gctest
%.gctest : %.pz $(TOP)/runtime/plzrun
	PZ_RUNTIME_DEV_OPTS=gc_zealous $(TOP)/runtime/plzrun $< > /dev/null

.PRECIOUS: %.out
%.out : %.pz $(TOP)/runtime/plzrun
	$(TOP)/runtime/plzrun $< > $@

.PHONY: clean
clean:
	rm -rf *.pz *.po *.out *.diff *.log

.PHONY: realclean
realclean: clean

