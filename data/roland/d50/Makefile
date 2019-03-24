%.names.text : %.syx
	hsc3-d50 sysex print all name $< > $@

%.group.text : %.syx
	hsc3-d50 sysex print all pp-group $< > $@

PN = PN-D50-00 PN-D50-01 PN-D50-02 PN-D50-03 PN-D50-04 PN-D50-05

all: $(PN:=.names.text) $(PN:=.group.text)

clean:
	rm -f *.text
