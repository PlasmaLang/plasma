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
	if $(TOP)/src/plzc --warnings-as-errors $< > $@ 2>&1 ; then \
		echo "Compilation succeeded" ; \
		echo "Compilation succeeded" >> $@ ; \
		false ; \
	fi

%.pzo : %.p $(TOP)/src/plzc
	$(TOP)/src/plzc $<

noentry.out : noentry.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n noentry -o noentry.pz $< > $@ 2>&1; \
	if [ $$? -eq 0 ] ; then false; else true; fi;

multi_entry.out : multi_entry.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n multi_entry -o multi_entry.pz $< > $@ 2>&1; \
	if [ $$? -eq 0 ] ; then false; else true; fi;

.PHONY: %.test
%.test : %.exp %.out
	diff -u $^ 

.PHONY: clean
clean:
	rm -rf *.pz *.out *.diff *.log *.pzo

.PHONY: realclean
realclean: clean
	rm -rf *.plasma-dump_*

