include ~/sw/hsc3-data/mk/dx7.mk

ROM = DX7-ROM1A DX7-ROM1B DX7-ROM2A DX7-ROM2B DX7-ROM3A DX7-ROM3B DX7-ROM4A DX7-ROM4B

all: $(ROM:=.syx.text) $(ROM:=.syx.names.text) $(ROM:=.syx.param.text)

clean:
	rm -f *.text
