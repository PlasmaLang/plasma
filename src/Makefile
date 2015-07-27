#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4
#

MMC_MAKE=mmc --make
MCFLAGS=--use-grade-subdirs

SOURCES=$(wildcard *.m)

all : tags pzasm pzrun

pzasm : $(SOURCES)
	$(MMC_MAKE) $(MCFLAGS) $@
	touch $@

pzrun : $(SOURCES)
	$(MMC_MAKE) $(MCFLAGS) $@
	touch $@

tags : $(SOURCES)
	mtags $(SOURCES)

.PHONY: clean
clean :
	rm -rf Mercury tags pzasm pzrun *.err *.mh

