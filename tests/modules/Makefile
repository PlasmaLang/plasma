#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# We don't normally use make to build Plasma programs, but it helps in this
# case where we want to check some of the individual tools work when called
# in "different" ways.
#
# vim: noet sw=4 ts=4
#

TOP=../..

.PHONY: all
all: module_03a.pzo module_03b.pzo module_03c.pzo

%.pi : %.p $(TOP)/src/plzc
	$(TOP)/src/plzc --mode make-interface $<

%.typeres : %.p $(TOP)/src/plzc
	$(TOP)/src/plzc --mode make-typeres-exports $<

%.pzo : %.p $(TOP)/src/plzc
	$(TOP)/src/plzc $<

%.pz : %.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n ${subst .pz,,$@} -o $@ $<

