#
# vim: noet sw=4 ts=4
#

SOURCES=$(wildcard *.m)

all : tags pzasm pzrun

pzasm : $(SOURCES)
	mmc --make $@

pzrun : $(SOURCES)
	mmc --make $@

tags : $(SOURCES)
	mtags $(SOURCES)

.PHONY: clean
clean :
	rm -rf Mercury tags pzasm pzrun *.err *.mh

