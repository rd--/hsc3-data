cmd =	ats au-to-pbm au-to-pgm \
	csv-mnd-to-pgm csv-plot csv-to-img \
	d50-sysex dx7-hex dx7-unpack \
	hex \
	img-edit img-query img-to-pbm img-to-pgm img-to-sf \
	kml-to-csv \
	lpc \
	midi \
	pbm-indices pbm-to-csv-mnd pbm-to-tbl pbm-trace \
	pgm-to-au \
	pvoc-header \
	sf-condense sf-draw sf-to-png \
	svl \
	text-monitor text-osc trace-rec-x11 \
	x11-ptr-to-midi-osc

include ~/sw/hsc3/mk/cmd.mk

# gcc-8.2.0 -O3 indicates no errors but unpack -> repack is not identity, -O works
dx7-unpack: dx7-unpack.c
	gcc -Wall -std=c99 -O -o dx7-unpack dx7-unpack.c

# VST

VST_SDK = $(HOME)/opt/src/vstsdk2.4

lxvst-stat: lxvst-stat.cpp
	g++ -Wall -I$(VST_SDK) -ldl -lpthread -o lxvst-stat lxvst-stat.cpp
