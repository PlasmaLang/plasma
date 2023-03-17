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
all: module_03a.pzo module_03b.pzo module_03c.pzo module_08.pzo \
	dyn_link_01_a.pz dyn_link_01.pz \
	dyn_link_02_ab.pz dyn_link_02.pz

%.pi : %.p $(TOP)/src/plzc
	$(TOP)/src/plzc --mode make-interface $<

%.typeres : %.p $(TOP)/src/plzc
	$(TOP)/src/plzc --mode make-typeres-exports $<

%.pzo : %.p $(TOP)/src/plzc
	$(TOP)/src/plzc $<

%.pz : %.pzo $(TOP)/src/plzlnk
	$(TOP)/src/plzlnk -n ${subst .pz,,$@} -o $@ $<

dyn_link_01.pzo : dyn_link_01_a.pi
dyn_link_01_a.pz : dyn_link_01_a.pzo
	$(TOP)/src/plzlnk -n DynLink01A --library -o $@ \
		dyn_link_01_a.pzo

dyn_link_02.pzo : dyn_link_02_a.pi dyn_link_02_b.pi
dyn_link_02_ab.pz : dyn_link_02_a.pzo dyn_link_02_b.pzo
	$(TOP)/src/plzlnk -n DynLink02A -n DynLink02B --library -o $@ \
		dyn_link_02_a.pzo dyn_link_02_b.pzo

