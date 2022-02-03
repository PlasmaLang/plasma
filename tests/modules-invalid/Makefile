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

.PRECIOUS: %.out
%.out : %.p $(TOP)/src/plzc
	if $(TOP)/src/plzc $< > $@ 2>&1 ; then \
		echo "Compilation succeeded" ; \
		echo "Compilation succeeded" >> $@ ; \
		false ; \
	fi

module_03.out : module_03a.pi
module_04a.out : module_04import.pi
module_04b.out : module_04import.pi
module_04c.out : module_04import.pi
module_04d.out : module_04import.pi

module_02a.pi: ;

%.pi : %.p $(TOP)/src/plzc
	$(TOP)/src/plzc --mode make-interface $<

%.pzo : %.p $(TOP)/src/plzc
	$(TOP)/src/plzc $<

%.pz : %.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n ${subst .pz,,$@} -o $@ $<

module_05.out : module_05.pzo module_05_.pzo $(TOP)/src/plzlnk
	if $(TOP)/src/plzlnk module_05.pzo module_05_.pzo \
			-n Module_05 -o module_05.pz > $@ 2>&1 ; then \
		echo "Linking succeeded" ; \
		echo "Linking succeeded" >> $@ ; \
		false ; \
	fi

module_06.out : module_06.p $(TOP)/src/plzc module_06a.pi

module_08.out : module_08.p module_08.c.p module_08.d.p BUILD.plz BINS
	if $(TOP)/src/plzbuild module_08 > $@ 2>&1 ; then \
		echo "Build succeeded" ; \
		echo "Build succeeded" >> $@ ; \
		false ; \
	fi
module_08b.out : module_08b.p module_08.c.p module_08.d.p BUILD.plz BINS
	if $(TOP)/src/plzbuild module_08b > $@ 2>&1 ; then \
		echo "Build succeeded" ; \
		echo "Build succeeded" >> $@ ; \
		false ; \
	fi
module_09_ref_unexported.out : module_09_ref_unexported.p module_09_ref_unexported_a.p BUILD.plz BINS
	if $(TOP)/src/plzbuild module_09_ref_unexported > $@ 2>&1 ; then \
		echo "Build succeeded" ; \
		echo "Build succeeded" >> $@ ; \
		false ; \
	fi

.PHONY: BINS
BINS: $(TOP)/src/plzc $(TOP)/src/plzlnk $(TOP)/src/plzbuild

.PHONY: %.test
%.test : %.exp %.outs
	diff -u $^

.PHONY: %.test
%.test : %.expish %.outs
	grep -cF -f $^

%.outs : %.out
	grep -v '^#' < $< | sed -e 's/#.*$$//' > $@

.PHONY: clean
clean:
	rm -rf *.pz *.pzo *.out *.diff *.log *.trace
	rm -f module_03a.pi module_04import.pi module_06a.pi
	rm -rf _build

.PHONY: realclean
realclean: clean
	rm -rf *.plasma-dump_*

