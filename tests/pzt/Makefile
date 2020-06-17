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

%.pzo : %.pzt $(TOP)/src/plzasm
	$(TOP)/src/plzasm $<

%.pzb : %.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n ${subst .pzb,,$@} -o $@ $<

link_01.pzb : link_01.pzo link_target_01.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n ${subst .pzb,,$@} -e link_01 -o $@ link_01.pzo link_target_01.pzo
link_02.pzb : link_02.pzo link_target_01.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n ${subst .pzb,,$@} -e link_02 -o $@ link_02.pzo link_target_01.pzo

.PHONY: %.test
%.test : %.exp %.out
	diff -u $^ 

.PHONY: %.gctest
%.gctest : %.pzb $(TOP)/runtime/plzrun
	PZ_RUNTIME_DEV_OPTS=gc_zealous $(TOP)/runtime/plzrun $< > /dev/null

.PRECIOUS: %.out
%.out : %.pzb $(TOP)/runtime/plzrun
	$(TOP)/runtime/plzrun $< > $@

.PHONY: clean
clean:
	rm -rf *.pzb *.pzo *.out *.diff *.log

.PHONY: realclean
realclean: clean

