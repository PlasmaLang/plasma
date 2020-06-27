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

%.pzo : %.p $(TOP)/src/plzc
	$(TOP)/src/plzc $<

%.pzb : %.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n ${subst .pzb,,$@} -o $@ $<

module_01.pzb : module_01.pzo module_01a.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_01 -e Module_01 -o $@ \
		module_01.pzo module_01a.pzo

.PHONY: %.test
%.test : %.exp %.outs
	diff -u $^

.PHONY: %.gctest
%.gctest : %.pzb $(TOP)/runtime/plzrun
	PZ_RUNTIME_DEV_OPTS=gc_zealous $(TOP)/runtime/plzrun $< > /dev/null

%.outs : %.out
	grep -v '^#' < $< | sed -e 's/#.*$$//' > $@

.PRECIOUS: %.out
%.out : %.pzb $(TOP)/runtime/plzrun
	$(TOP)/runtime/plzrun $< > $@

# This target can be used form manual testing, eg to look for differences
# between traces of the same program.
%.trace: %.pzb $(TOP)/runtime/plzrun
	 PZ_RUNTIME_DEV_OPTS=interp_trace $(TOP)/runtime/plzrun $< 2>&1 | sed -e 's/0x[0-9a-f]\+/0xXXXX/g' > $@

.PHONY: clean
clean:
	rm -rf *.pzb *.pzo *.out *.diff *.log *.trace

.PHONY: realclean
realclean: clean
	rm -rf *.plasma-dump_*

