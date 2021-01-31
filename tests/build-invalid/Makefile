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

bad_module_name.out : bad_module_name.build $(TOP)/src/plzbuild
	if PATH=$(abspath $(TOP))/src/:$$PATH \
			$(TOP)/src/plzbuild --rebuild --build-file $< \
			"quack!!bad," GoodName "" \
			> $@ 2>&1 ; then \
		echo "Compilation succeeded" ; \
		echo "Compilation succeeded" >> $@ ; \
		false ; \
	fi

bad_file_2.out : $(TOP)/src/plzbuild
	if PATH=$(abspath $(TOP))/src/:$$PATH \
			$(TOP)/src/plzbuild --rebuild --build-file not_exist.build \
			> $@ 2>&1 ; then \
		echo "Compilation succeeded" ; \
		echo "Compilation succeeded" >> $@ ; \
		false ; \
	fi

.PRECIOUS: %.out
%.out : %.build $(TOP)/src/plzbuild extra_module.p
	if PATH=$(abspath $(TOP))/src/:$$PATH \
			$(TOP)/src/plzbuild --rebuild --build-file $< \
			> $@ 2>&1 ; then \
		echo "Compilation succeeded" ; \
		echo "Compilation succeeded" >> $@ ; \
		false ; \
	fi

include_file_nobuild.out: include_file_nobuild.p
include_nofile_build.out: include_nofile_build.p
include_nofile_nobuild.out: include_nofile_nobuild.p
file_in_other_program.out: file_in_other_program.p other_program.p

.PHONY: %.test
%.test : %.exp %.out
	diff -u $^ 

.PHONY: %.test
%.test : %.expish %.out
	grep -cF -f $^

.PHONY: clean
clean:
	rm -rf *.pz *.out *.diff *.log _build

