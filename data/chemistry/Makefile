J=$(HOME)/rd/j
P=$(HOME)/data/chemistry
Q=$(HOME)/sw/hsc3-data/data/chemistry

ln-cls:
	mkdir -p $(Q)/cls
	rm -f $(Q)/cls/xyz
	ln -s $(J)/2018-09-26/xyz $(Q)/cls/xyz

ln-aflow:
	rm -f $(Q)/aflow/poscar
	ln -s $(P)/aflow/poscar $(Q)/aflow

ln-mp:
	rm -f $(Q)/mp/poscar
	ln -s $(P)/mp/poscar $(Q)/mp
