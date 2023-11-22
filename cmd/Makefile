cmd =	chemistry \
	data d50 dx7 dx7-unpack \
	img-edit img-to-sf \
	kml-to-csv \
	lpc \
	midi \
	pbm-to-csv-mnd pbm-trace \
	pdb \
	pvoc \
	sf-condense sf-draw sf-to-png \
	svl \
	text-monitor text-osc \
	x11

include ~/sw/hsc3-meta/mk/cmd.mk

dx7-unpack: dx7-unpack.c
	gcc -Wall -std=c99 -O2 -o dx7-unpack dx7-unpack.c r-common/c/lib-r-common.a

# VST

VST_SDK = $(HOME)/opt/src/vstsdk2.4

lxvst-stat: lxvst-stat.cpp
	g++ -Wall -I$(VST_SDK) -ldl -lpthread -o lxvst-stat lxvst-stat.cpp

mk-r-common:
	git submodule update r-common
	(cd r-common/c ; make)

mk-d50:
	(rm -f d50 ; make d50 ; cp d50 $(prefix)/bin/hsc3-d50)

mk-dx7:
	(rm -f dx7 ; make dx7 ; cp dx7 $(prefix)/bin/hsc3-dx7)

mk-dx7-unpack:
	(rm -f dx7-unpack ; make dx7-unpack ; cp dx7-unpack $(prefix)/bin/hsc3-dx7-unpack)

mk-pdb:
	(rm -f pdb ; make pdb ; cp pdb $(prefix)/bin/hsc3-pdb)

dep:
	cabal install hscurses
