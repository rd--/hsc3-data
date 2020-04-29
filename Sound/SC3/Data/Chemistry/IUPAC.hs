-- | International Union of Pure and Applied Chemistry <https://iupac.org/>
module Sound.SC3.Data.Chemistry.IUPAC where

import Data.Char {- base -}
import Data.List {- base -}

{- | (IUPAC-CODE,BASES-REPRESENTED)

<https://www.bioinformatics.org/sms/iupac.html>
-}
iupac_nucleotide_tbl :: [(Char,[Char])]
iupac_nucleotide_tbl =
  [('A',"A"),('C',"C"),('G',"G"),('T',"T")
  ,('U',"U")
  ,('R',"AG"),('Y',"CT")
  ,('S',"GC"),('W',"AT")
  ,('K',"GT"),('M',"AC")
  ,('B',"CGT"),('D',"AGT"),('H',"ACT"),('V',"ACG")
  ,('N',"ACGT")
  ,('-',""),('.',"")]

{- | (IUPAC-CODE,THREE-LETTER-CODE,DESCRIPTION)

<https://www.bioinformatics.org/sms/iupac.html>

> length iupac_amino_acid_tbl == 22
-}
iupac_amino_acid_tbl :: [(Char,String,String)]
iupac_amino_acid_tbl =
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
  ,('Y',"Tyr","Tyrosine")
  -- EXT
  ,('U',"Sec","Selenocysteine")
  ,('O',"Pyl","Pyrrolysine")]

-- | Translate from 1-letter IUPAC code (upper-case only) to 3-letter IUPAC code.
iupac_one_letter_code_to_three_letter_code :: Char -> Maybe String
iupac_one_letter_code_to_three_letter_code x =
  let f (c1,_,_) = x == c1
      g (_,c3,_) = c3
  in fmap g (find f iupac_amino_acid_tbl)

-- | Translate from 3-letter IUPAC code (case insensitive) to 1-letter IUPAC code.
--
-- > iupac_three_letter_code_to_one_letter_code "GLY" == Just 'G'
iupac_three_letter_code_to_one_letter_code :: String -> Maybe Char
iupac_three_letter_code_to_one_letter_code x =
  let ci p q = map toUpper p == map toUpper q
      f (_,c3,_) = ci x c3
      g (c1,_,_) = c1
  in fmap g (find f iupac_amino_acid_tbl)
