# pdb

`title` and `header` print the TITLE (joined) and HEADER records.

~~~~
$ hsc3-pdb title ~/data/pdb/structure/1a6m.pdb
1A6M - OXY-MYOGLOBIN, ATOMIC RESOLUTION
$ hsc3-pdb header ~/data/pdb/structure/1ah6.pdb
1AH6 - 14-APR-97 - CHAPERONE
$
~~~~

`stat` prints summary information for a PDB file.

~~~~
$ hsc3-pdb stat ~/data/pdb/structure/1a6m.pdb
ID: 1A6M
CLASSIFICATION: OXYGEN TRANSPORT
DEPOSITION-DATE: 26-FEB-98
TITLE: OXY-MYOGLOBIN, ATOMIC RESOLUTION
NUMMDL: 1
N-ATOM: 1336
N-HETATM: 248
ATOM-ALT-ID: AB
N-ATOM-ALT: 278
N-CHAIN: 1
N-UNIQ-CHAIN: 1
CHAIN-ID-SEQ: A
UNIQ-CHAIN-ID-SEQ: A
N-ELEMENTS: 5
ELEMENTS: C FE N O S
N-CONECT: 62
N-LINK: 3
N-SSBOND: 0
N-HELIX: 8
N-SHEET: 0
SEQRES-N: 151
RES-N: 341
HOH-N: 186
$
~~~~

`seqres` and `seqres-iupac` print collated SEQRES records.

~~~~
$ hsc3-pdb seqres ~/data/pdb/structure/1e9w.pdb
1E9W
A: QUA ILE ALA DHA ALA SER BB9 THR DBU DCY TS9 BB9 THR BB9 MH6 BB9 DHA DHA NH2

$ hsc3-pdb seqres-iupac ~/data/pdb/structure/2dgc.pdb
2DGC
A: MIVPESSDPAALKRARNTEAARRSRARKLQRMKQLEDKVEELLSKNYHLENEVARLKKLVGER
B: TGGAGATGACGTCATCTCC

$
~~~~
