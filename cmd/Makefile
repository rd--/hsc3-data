cmd =	ats au-to-pbm au-to-pgm \
	csv-mnd-to-midi csv-mnd-to-pgm csv-plot csv-to-img \
	d50 dx7 dx7-unpack \
	hex \
	img-edit img-query img-to-pbm img-to-pgm img-to-sf \
	kml-to-csv \
	lpc \
	midi \
	pbm-indices pbm-to-csv-mnd pbm-to-tbl pbm-trace \
	pdb \
	pgm-to-au \
	pvoc \
	sf-condense sf-draw sf-to-png \
	svl \
	text-monitor text-osc trace-rec-x11 \
	wv \
	x11-ptr-to-midi-osc

include ~/sw/hsc3/mk/cmd.mk

dx7-unpack: dx7-unpack.c
	gcc -Wall -std=c99 -O2 -o dx7-unpack dx7-unpack.c c-common/lib-c-common.a

WV_OPT = -I ~/opt/src/webview `pkg-config --cflags --libs gtk+-3.0 webkit2gtk-4.0`

wv: wv.c
	g++ $(WV_OPT) wv.c -o wv

# VST

VST_SDK = $(HOME)/opt/src/vstsdk2.4

lxvst-stat: lxvst-stat.cpp
	g++ -Wall -I$(VST_SDK) -ldl -lpthread -o lxvst-stat lxvst-stat.cpp

ln-local-c-common:
	rm -f c-common
	ln -s $(HOME)/sw/c-common c-common

mk-local-c-common:
	(cd c-common ; make)

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
