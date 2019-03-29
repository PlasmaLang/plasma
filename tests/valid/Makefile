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

%.pz : %.p $(TOP)/src/plzc
	$(TOP)/src/plzc $<
	$(TOP)/src/plzc --dump-stages --no-write-output $<

.PHONY: %.test
%.test : %.exp %.outs
	diff -u $^ 

.PHONY: %.gctest
%.gctest : %.pz $(TOP)/runtime/pzrun
	PZ_RUNTIME_DEV_OPTS=gc_zealous $(TOP)/runtime/pzrun $< > /dev/null

%.outs : %.out
	grep -v '^#' < $< > $@

.PRECIOUS: %.out
%.out : %.pz $(TOP)/runtime/pzrun
	$(TOP)/runtime/pzrun $< > $@

die.out : die.pz $(TOP)/runtime/pzrun
	$(TOP)/runtime/pzrun $< > $@ 2>&1; \
	if [ $$? -eq 0 ] ; then false; else true; fi;

.PHONY: clean
clean:
	rm -rf *.pz *.out *.diff *.log

.PHONY: realclean
realclean: clean
	rm -rf *.plasma-dump_*

