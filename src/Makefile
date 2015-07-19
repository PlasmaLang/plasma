#
# vim: noet sw=4 ts=4
#

MMC_MAKE=mmc --make
MCFLAGS=--use-grade-subdirs

SOURCES=$(wildcard *.m)

all : tags pzasm pzrun

pzasm : $(SOURCES)
	$(MMC_MAKE) $(MCFLAGS) $@

pzrun : $(SOURCES)
	$(MMC_MAKE) $(MCFLAGS) $@

tags : $(SOURCES)
	mtags $(SOURCES)

.PHONY: clean
clean :
	rm -rf Mercury tags pzasm pzrun *.err *.mh

