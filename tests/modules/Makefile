#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4
#

TOP=../..

TOOLS=$(TOP)/src/plzc $(TOP)/src/plzlnk $(TOP)/src/plzbuild

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

%.pz : %.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n ${subst .pz,,$@} -o $@ $<

module_01.pz : module_01.p module_01a.p BUILD.plz $(TOOLS)
	$(TOP)/src/plzbuild module_01

module_01_dyn.out : module_01.pz module_02.pz
	$(TOP)/runtime/plzrun -l module_02.pz module_01.pz > $@

.PHONY: module_01_dyn.gctest
module_01_dyn.gctest : module_01.pz module_02.pz $(TOP)/runtime/plzrun
	PZ_RUNTIME_DEV_OPTS=gc_zealous $(TOP)/runtime/plzrun -l module_02.pz \
		module_01.pz > /dev/null

module_02.pzo : module_02a.pi
module_02a.pzo : module_02.pi

module_02.pz : module_02.pzo module_02a.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_02 -o $@ \
		module_02.pzo module_02a.pzo

#
# Create four tests, the first to link modules a and b, the last to link b
# and a (reverse order), they choose different entry modules.
module_03a.pz : module_03a.pzo module_03b.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_03a -e Module_03a.main -o $@ \
		module_03a.pzo module_03b.pzo
module_03b.pz : module_03a.pzo module_03b.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_03b -e Module_03b.main -o $@ \
		module_03a.pzo module_03b.pzo
module_03ar.pz : module_03a.pzo module_03b.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_03ar -e Module_03a.main -o $@ \
		module_03b.pzo module_03a.pzo
module_03br.pz : module_03a.pzo module_03b.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_03br -e Module_03b.main -o $@ \
		module_03b.pzo module_03a.pzo

# module_03c.pzo does not have an entrypoint.  The linker should implicitly
# choose the only entrypoint.
module_03c.pz : module_03a.pzo module_03c.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_03c -o $@ \
		module_03a.pzo module_03c.pzo
module_03cr.pz : module_03a.pzo module_03c.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_03c -o $@ \
		module_03c.pzo module_03a.pzo

module_04.pz : module_04.p module_04a.p BUILD.plz $(TOOLS) 
	$(TOP)/src/plzbuild module_04

module_05.pz : module_05.p module_05a.p BUILD.plz $(TOOLS) 
	$(TOP)/src/plzbuild module_05

module_06.pz : module_06.p module_06a.p BUILD.plz $(TOOLS)
	$(TOP)/src/plzbuild module_06

module_07.pz : module_07.p module_07a.p BUILD.plz $(TOOLS)
	$(TOP)/src/plzbuild module_07

module_08a.pz : module_08.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_08a -e Module_08.name1 -o $@ \
		module_08.pzo
module_08b.pz : module_08.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n Module_08a -e Module_08.name2 -o $@ \
		module_08.pzo

module_09.pzo : module_09a.pi
module_09a.pz : module_09a.pzo
	$(TOP)/src/plzlnk -n Module_09a --library -o $@ \
		module_09a.pzo
module_09.out : module_09.pz module_09a.pz $(TOP)/runtime/plzrun
	$(TOP)/runtime/plzrun -l module_09a.pz module_09.pz > $@
.PHONY: module_09.gctest
module_09.gctest : module_09.pz module_09a.pz $(TOP)/runtime/plzrun
	PZ_RUNTIME_DEV_OPTS=gc_zealous \
		$(TOP)/runtime/plzrun -l module_09a.pz module_09.pz > /dev/null

module_10.pzo : module_10a.pi module_10b.pi
module_10ab.pz : module_10a.pzo module_10b.pzo
	$(TOP)/src/plzlnk -n Module_10a -n Module_10b --library -o $@ \
		module_10a.pzo module_10b.pzo
module_10.out : module_10.pz module_10ab.pz $(TOP)/runtime/plzrun
	$(TOP)/runtime/plzrun -l module_10ab.pz module_10.pz > $@
.PHONY: module_10.gctest
module_10.gctest : module_10.pz module_10ab.pz $(TOP)/runtime/plzrun
	PZ_RUNTIME_DEV_OPTS=gc_zealous \
		$(TOP)/runtime/plzrun -l module_10ab.pz module_10.pz > /dev/null 

heir_test.pz : heir_test.p heir.foo.p heir.foo_bar.p BUILD.plz $(TOOLS)
	$(TOP)/src/plzbuild heir_test

.PHONY: %.test
%.test : %.exp %.outs
	diff -u $^

.PHONY: %.gctest
%.gctest : %.pz $(TOP)/runtime/plzrun
	PZ_RUNTIME_DEV_OPTS=gc_zealous $(TOP)/runtime/plzrun $< > /dev/null

%.outs : %.out
	grep -v '^#' < $< | sed -e 's/#.*$$//' > $@

%.out : %.pz $(TOP)/runtime/plzrun
	$(TOP)/runtime/plzrun $< > $@

# This target can be used form manual testing, eg to look for differences
# between traces of the same program.
%.trace: %.pz $(TOP)/runtime/plzrun
	 PZ_RUNTIME_DEV_OPTS=interp_trace $(TOP)/runtime/plzrun $< 2>&1 | sed -e 's/0x[0-9a-f]\+/0xXXXX/g' > $@

.PHONY: clean
clean:
	rm -rf *.pi *.pz *.pzo *.out *.diff *.log *.trace
	rm -rf _build

.PHONY: realclean
realclean: clean
	rm -rf *.plasma-dump_*

