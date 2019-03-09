include ~/sw/hsc3-data/mk/dx7.mk

VRC = VRC-101-A VRC-101-B VRC-102-A VRC-102-B VRC-103-A VRC-103-B VRC-104-A VRC-104-B VRC-105-A VRC-105-B VRC-106-A VRC-106-B VRC-107-A VRC-107-B VRC-108-A VRC-108-B VRC-109-A VRC-109-B VRC-110-A VRC-110-B VRC-111-A VRC-111-B VRC-112-A VRC-112-B

all: $(VRC:=.syx.text) $(VRC:=.syx.names.text) $(VRC:=.syx.param.text)

clean:
	rm -f *.text
