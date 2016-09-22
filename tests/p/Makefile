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

%.pz : %.p $(TOP)/src/plasmac
	$(TOP)/src/plasmac $<

.PHONY: %.test
%.test : %.exp %.out
	diff -u $^ 

.PRECIOUS: %.out
%.out : %.pz $(TOP)/runtime/pzrun
	$(TOP)/runtime/pzrun $< > $@

.PHONY: clean
clean:
	rm -rf *.pz *.out *.diff *.log

