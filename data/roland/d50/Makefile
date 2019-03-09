%.syx.names.text : %.syx
	hsc3-d50-sysex print-sysex all name $< > $@

PN = PN-D50-00 PN-D50-01 PN-D50-02 PN-D50-03 PN-D50-04 PN-D50-05

all: $(PN:=.syx.names.text)

clean:
	rm -f *.text
