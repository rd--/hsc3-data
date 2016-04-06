cmd =	ats au-to-pbm au-to-pgm \
	csv-mnd-to-pgm csv-plot \
	img-edit img-to-pbm img-to-pgm img-to-sf \
	kml-to-csv \
	midi-to-csv-mnd midi-to-text \
	pbm-indices pbm-to-csv-mnd pbm-to-tbl pbm-trace \
	pgm-to-au \
	pvoc-header \
	sf-condense sf-draw sf-to-png \
	text-monitor text-osc trace-rec-x11 \
	x11-ptr-to-midi-osc

include ~/sw/hsc3/mk/cmd.mk

VST_SDK = $(HOME)/opt/src/vstsdk2.4

lxvst-stat: lxvst-stat.cpp
	g++ -Wall -I$(VST_SDK) -ldl -lpthread -o lxvst-stat lxvst-stat.cpp

