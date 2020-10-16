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

%.pz : %.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n ${subst .pz,,$@} -o $@ $<

link_01.pz : link_01.pzo link_target_01.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n ${subst .pz,,$@} -e link_01.main_closure -o $@ link_01.pzo link_target_01.pzo
link_02.pz : link_02.pzo link_target_01.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n ${subst .pz,,$@} -e link_02.main_closure -o $@ link_02.pzo link_target_01.pzo
link_03.pz : link_03.pzo link_target_02.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n ${subst .pz,,$@} -e link_03.main_closure -o $@ link_03.pzo link_target_02.pzo

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
	rm -rf *.pz *.pzo *.out *.diff *.log

.PHONY: realclean
realclean: clean

