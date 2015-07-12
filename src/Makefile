#
# vim: noet sw=4 ts=4
#

SOURCES=$(wildcard *.m)

all : tags pzasm

pzasm : $(SOURCES)
	mmc --make $@

tags : $(SOURCES)
	mtags $(SOURCES)

.PHONY: clean
clean :
	rm -rf Mercury tags pzasm *.err *.mh

