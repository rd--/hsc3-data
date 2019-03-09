include ~/sw/hsc3-data/mk/dx7.mk

I = TX816-TFI1 TX816-TFI2 TX816-TFI3 TX816-TFI4 TX816-TFI5 TX816-TFI6 TX816-TFI7 TX816-TFI8
R = TX816-TFR1 TX816-TFR2
TF = $(I) $(R)

all: $(TF:=.syx.text) $(TF:=.syx.names.text) $(TF:=.syx.param.text)

clean:
	rm -f *.text
