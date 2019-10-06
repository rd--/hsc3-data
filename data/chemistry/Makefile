J=$(HOME)/rd/j
Q=$(HOME)/sw/hsc3-data/data/chemistry

ln-cls:
	mkdir -p $(Q)/cls
	ln -s $(J)/2018-09-26/xyz $(Q)/cls/xyz

ln-aflow:
	mkdir -p $(Q)/aflow
	ln -s $(J)/2018-09-27/poscar $(Q)/aflow/poscar

ln-pubchem:
	mkdir -p $(Q)/pubchem
	ln -s $(J)/2018-09-28/mol $(Q)/pubchem/mol

ln-mp:
	mkdir -p $(Q)/mp
	ln -s $(J)/2018-10-11/poscar $(Q)/mp/poscar

all:
	make clean
	make ln-cls ln-aflow ln-pubchem ln-mp

clean:
	rm -f $(Q)/cls/xyz
	rm -f $(Q)/aflow/poscar
	rm -f $(Q)/pubchem/mol
	rm -f $(Q)/mp/poscar
