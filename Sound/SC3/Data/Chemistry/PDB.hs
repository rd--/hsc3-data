-- | Protein Data Bank, <https://www.rcsb.org/>
module Sound.SC3.Data.Chemistry.PDB where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

{- | (IUPAC-CODE,THREE-LETTER-CODE,DESCRIPTION)

<https://www.bioinformatics.org/sms/iupac.html>
-}
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

-- | Lookup PDB SEQRES code in 'proteinogenic_amino_acid_tbl'.
pdb_seqres_code_lookup :: String -> Maybe (Char,String)
pdb_seqres_code_lookup x =
  let f (_,c3,_) = x == map toUpper c3
      g (c1,_,dsc) = (c1,dsc)
  in fmap g (find f proteinogenic_amino_acid_tbl)

{- | Erroring variant.

> let s = "ALA CYS ASP GLU PHE GLY HIS ILE LYS LEU MET ASN PRO GLN ARG SER THR VAL TRP TYR"
> map (fst . pdb_seqres_code_lookup_err) (words s) == "ACDEFGHIKLMNPQRSTVWY"
-}
pdb_seqres_code_lookup_err :: String -> (Char, String)
pdb_seqres_code_lookup_err = fromMaybe (error "pdb_seqres_code_lookup?") . pdb_seqres_code_lookup

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

-- * URI

pdb_ligand_summary_uri :: String -> String
pdb_ligand_summary_uri k = "http://www.rcsb.org/ligand/" ++ k

pdb_ligand_ideal_sdf_uri :: String -> String
pdb_ligand_ideal_sdf_uri k = concat ["http://files.rcsb.org/ligands/view/",k,"_ideal.sdf"]

{-
https://pdb101.rcsb.org/learn/guide-to-understanding-pdb-data/small-molecule-ligands
-}
