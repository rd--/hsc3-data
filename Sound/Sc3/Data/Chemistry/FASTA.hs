-- | FASTA <https://www.ncbi.nlm.nih.gov/BLAST/fasta.shtml>
module Sound.Sc3.Data.Chemistry.FASTA where

-- | (IUPAC-CODE,BASES-REPRESENTED,DESCRIPTION)
fasta_nucleic_acid_tbl :: [(Char, [Char], String)]
fasta_nucleic_acid_tbl =
  [('A',"A","adenosine")
  ,('C',"C","cytidine")
  ,('G',"G","guanine")
  ,('T',"T","thymidine")
  ,('N',"AGCT","any")
  ,('U',"U","uridine")
  -- DEGENERATE
  ,('K',"GT","keto")
  ,('S',"GC","strong")
  ,('Y',"TC","pyrimidine")
  ,('M',"AC","amino")
  ,('W',"AT","weak")
  ,('R',"GA","purine")
  ,('B',"GTC","")
  ,('D',"GAT","")
  ,('H',"ACT","")
  ,('V',"GCA","")
  ,('-',"","gap of indeterminate length")]

fasta_nucleic_acid_dict :: [Char]
fasta_nucleic_acid_dict = map (\(x,_,_) -> x) fasta_nucleic_acid_tbl

-- | (IUPAC-CODE,DESCRIPTION)
fasta_amino_acid_code_tbl :: [(Char,String)]
fasta_amino_acid_code_tbl =
  [('A',"alanine")
  ,('B',"aspartate/asparagine")
  ,('C',"cystine")
  ,('D',"aspartate")
  ,('E',"glutamate")
  ,('F',"phenylalanine")
  ,('G',"glycine")
  ,('H',"histidine")
  ,('I',"isoleucine")
  ,('K',"lysine")
  ,('L',"leucine")
  ,('M',"methionine")
  ,('N',"asparagine")
  ,('P',"proline")
  ,('Q',"glutamine")
  ,('R',"arginine")
  ,('S',"serine")
  ,('T',"threonine")
  ,('U',"selenocysteine")
  ,('V',"valine")
  ,('W',"tryptophan")
  ,('X',"any")
  ,('Y',"tyrosine")
  ,('Z',"glutamate/glutamine")
  ,('-',"gap of indeterminate length")
  ,('*',"translation stop")]

fasta_amino_acid_dict :: [Char]
fasta_amino_acid_dict = map fst fasta_amino_acid_code_tbl
