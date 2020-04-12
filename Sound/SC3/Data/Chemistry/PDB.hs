module Sound.SC3.Data.Chemistry.PDB where

-- | (IUPAC-CODE,THREE-LETTER-CODE,DESCRIPTION)
--
-- <https://www.bioinformatics.org/sms/iupac.html>
proteinogenic_amino_acid_tbl :: [(Char,String,String)]
proteinogenic_amino_acid_tbl =
  [('A',"Ala","Alanine")
  ,('C',"Cys","Cysteine")
  ,('D',"Asp","Aspartic Acid")
  ,('E',"Glu","Glutamic Acid")
  ,('F',"Phe","Phenylalanine")
  ,('G',"Gly","Glycine")
  ,('H',"His","Histidine")
  ,('I',"Ile","Isoleucine")
  ,('K',"Lys","Lysine")
  ,('L',"Leu","Leucine")
  ,('M',"Met","Methionine")
  ,('N',"Asn","Asparagine")
  ,('P',"Pro","Proline")
  ,('Q',"Gln","Glutamine")
  ,('R',"Arg","Arginine")
  ,('S',"Ser","Serine")
  ,('T',"Thr","Threonine")
  ,('V',"Val","Valine")
  ,('W',"Trp","Tryptophan")
  ,('Y',"Tyr","Tyrosine")]

-- | (IUPAC-CODE,DESCRIPTION,COMPLEMENT)
nucleotide :: [(Char,String,Char)]
nucleotide =
  [('A',"Adenine",'T')
  ,('C',"Cytosine",'G')
  ,('G',"Guanine",'C')
  ,('T',"Thymine",'A')]

-- | (IUPAC-CODE,DESCRIPTION,COMPLEMENT)
--
-- <https://www.bioinformatics.org/sms/iupac.html>
nucleotide_iupac :: [(Char, String, Char)]
nucleotide_iupac =
  [('A',"Adenine",'T')
  ,('C',"Cytosine",'G')
  ,('G',"Guanine",'C')
  ,('T',"Thymine",'A')
  ,('U',"Uracil",'A')
  ,('W',"Weak",'W')
  ,('S',"Strong",'S')
  ,('M',"aMino",'K')
  ,('K',"Keto",'M')
  ,('R',"puRine",'Y')
  ,('Y',"pYrimidine",'R')
  ,('B',"not A",'V')
  ,('D',"not C",'H')
  ,('H',"not G",'D')
  ,('V',"not T",'B')
  ,('N',"any",'N')
  ,('-',"Gap (Zero)",'-')
  ,('.',"Gap (Zero)",'.')]
