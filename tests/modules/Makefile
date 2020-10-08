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

%.pi : %.p $(TOP)/src/plzc
	$(TOP)/src/plzc --make-interface $<

%.pzo : %.p $(TOP)/src/plzc
	$(TOP)/src/plzc $<

%.pzb : %.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n ${subst .pzb,,$@} -o $@ $<

module_01.pzo : module_01a.pi

module_01.pzb : module_01.pzo module_01a.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_01 -o $@ \
		module_01.pzo module_01a.pzo

module_02.pzo : module_02a.pi
module_02a.pzo : module_02.pi

module_02.pzb : module_02.pzo module_02a.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_02 -o $@ \
		module_02.pzo module_02a.pzo

#
# Create four tests, the first to link modules a and b, the last to link b
# and a (reverse order), they choose different entry modules.
module_03a.pzb : module_03a.pzo module_03b.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_03a -e Module_03a -o $@ \
		module_03a.pzo module_03b.pzo
module_03b.pzb : module_03a.pzo module_03b.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_03b -e Module_03b -o $@ \
		module_03a.pzo module_03b.pzo
module_03ar.pzb : module_03a.pzo module_03b.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_03ar -e Module_03a -o $@ \
		module_03b.pzo module_03a.pzo
module_03br.pzb : module_03a.pzo module_03b.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_03br -e Module_03b -o $@ \
		module_03b.pzo module_03a.pzo

# module_03c.pzo does not have an entrypoint.  The linker should implicitly
# choose the only entrypoint.
module_03c.pzb : module_03a.pzo module_03c.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_03c -o $@ \
		module_03a.pzo module_03c.pzo
module_03cr.pzb : module_03a.pzo module_03c.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_03c -o $@ \
		module_03c.pzo module_03a.pzo

module_04.pzo : module_04a.pi

module_04.pzb : module_04.pzo module_04a.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_04 -o $@ \
		module_04.pzo module_04a.pzo

module_05.pzo : module_05a.pi

module_05.pzb : module_05.pzo module_05a.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_05 -o $@ \
		module_05.pzo module_05a.pzo

module_06.pzo : module_06a.pi

module_06.pzb : module_06.pzo module_06a.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_06 -o $@ \
		module_06.pzo module_06a.pzo

module_07.pzo : module_07a.pi

module_07.pzb : module_07.pzo module_07a.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_07 -o $@ \
		module_07.pzo module_07a.pzo

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
	rm -rf *.pi *.pzb *.pzo *.out *.diff *.log *.trace

.PHONY: realclean
realclean: clean
	rm -rf *.plasma-dump_*

