{- | Pdb = Protein Data Bank = <http://www.wwpdb.org/>

Research Collaboratory for Structural Bioinformatics Pdb = <https://www.rcsb.org/>
Pdbj (Protein Data Bank Japan) = <https://pdbj.org/>
Pdbe (Protein Data Bank in Europe) = <https://www.ebi.ac.uk/pdbe/>
Biological Magnetic Resonance Data Bank = <http://www.bmrb.wisc.edu/>
-}
module Sound.Sc3.Data.Chemistry.Pdb where

import qualified Data.Char {- base -}

import qualified System.Directory {- directory -}
import qualified System.FilePath {- filepath -}
import qualified System.Process {- process -}

import qualified Music.Theory.Directory as Directory {- hmt-base -}
import qualified Music.Theory.Maybe as Maybe {- hmt-base -}

import qualified Sound.Sc3.Data.Chemistry.Iupac as Iupac {- hsc3-data -}
import qualified Sound.Sc3.Data.Chemistry.Pdb.Parse as Pdb.Parse {- hsc3-data -}
import qualified Sound.Sc3.Data.Chemistry.Pdb.Types as Pdb.Types {- hsc3-data -}

-- * Amino Acid and Nucleotide Nomenclature

{- | The standard Pdb codes for amino acids, deoxyribonucleotides and ribonucleotides.

- <https://en.wikipedia.org/wiki/Amino_acid>
- <https://en.wikipedia.org/wiki/Deoxyribonucleotide>
- <https://en.wikipedia.org/wiki/Ribonucleotide>
-}
pdb_std_codes :: ([String], [String], [String])
pdb_std_codes =
  ( words "ALA CYS ASP GLU PHE GLY HIS ILE LYS LEU MET ASN PRO GLN ARG SER THR VAL TRP TYR"
  , words "DA DC DG DT DI"
  , words "A C G U I"
  )

{- | Is /x/ a standard Pdb code?

>>> all pdb_is_std_code (words "LEU DT U")
True
-}
pdb_is_std_code :: String -> Bool
pdb_is_std_code x =
  let (a, d, r) = pdb_std_codes
  in case length x of
      1 -> x `elem` r
      2 -> x `elem` d
      3 -> x `elem` a
      _ -> False

-- | The standard 3-character codes for Amino Acids.
pdb_amino_acids :: [String]
pdb_amino_acids = let (a, _, _) = pdb_std_codes in a

-- | The standard 2-character codes for Deoxyribonucleotides.
pdb_deoxyribonucleotides :: [String]
pdb_deoxyribonucleotides = let (_, d, _) = pdb_std_codes in d

-- | The standard 1-character codes for Ribonucleotides.
pdb_ribonucleotides :: [String]
pdb_ribonucleotides = let (_, _, r) = pdb_std_codes in r

-- | The standard 2- and 1-character codes for nucleotides.
pdb_nucleotides :: [String]
pdb_nucleotides = let (_, d, r) = pdb_std_codes in d ++ r

-- | (Pdb-Code,Iupac-Code)
pdb_code_tbl :: [(String, Char)]
pdb_code_tbl =
  concat
    [ map (\(c1, c3, _) -> (map Data.Char.toUpper c3, c1)) Iupac.iupac_amino_acid_tbl
    , map (\x -> (x, last x)) pdb_nucleotides
    ]

{- | Translate Pdb SEQRES code (upper case 3-letter code) to IUPAC code.

>>> pdb_seqres_code_lookup "LEU"
Just 'L'
-}
pdb_seqres_code_lookup :: String -> Maybe Char
pdb_seqres_code_lookup = flip lookup pdb_code_tbl

{- | Erroring variant.

>>> pdb_seqres_code_lookup_err "GLY"
'G'

>>> map pdb_seqres_code_lookup_err pdb_amino_acids
"ACDEFGHIKLMNPQRSTVWY"
-}
pdb_seqres_code_lookup_err :: String -> Char
pdb_seqres_code_lookup_err =
  Maybe.from_just "pdb_seqres_code_lookup"
    . pdb_seqres_code_lookup

-- * Convert

{- | Run obabel process to convert Pdb file to Mol file.

Pdb files are converted to Mol files using obabel,
<https://packages.debian.org/stable/openbabel>
-}
pdb_to_mol :: FilePath -> FilePath -> IO ()
pdb_to_mol pdb_fn mol_fn =
  System.Process.callProcess
    "obabel"
    ["-ipdb", pdb_fn, "-omol", "-O", mol_fn]

-- | Variant that only runs if the Mol file does not already exist.
pdb_to_mol_x :: FilePath -> FilePath -> IO ()
pdb_to_mol_x pdb_fn mol_fn = do
  System.Directory.createDirectoryIfMissing True (System.FilePath.takeDirectory mol_fn)
  Directory.if_file_exists (mol_fn, return (), pdb_to_mol pdb_fn mol_fn)

-- * File-Names

{- | Pdb filenames are lower case, with a .pdb extension. Identifiers are upper-case.

>>> pdb_file_name_to_id "rscb/1poc.pdb"
"1POC"
-}
pdb_file_name_to_id :: FilePath -> String
pdb_file_name_to_id =
  map Data.Char.toUpper
    . System.FilePath.dropExtension
    . System.FilePath.takeFileName

{- | Filename for ligand /k/, /ty/ is "ideal" or "model"

>>> pdb_ligand_sdf_filename "ideal" "GLY"
"GLY_ideal.sdf"
-}
pdb_ligand_sdf_filename :: String -> String -> String
pdb_ligand_sdf_filename ty k = concat [k, "_", ty, ".sdf"]

-- * Rcsb-Uri

-- | Uri for structure summary.
pdb_structure_summary_uri :: String -> String
pdb_structure_summary_uri = (++) "http://www.rcsb.org/structure/"

-- | Uri for structure Pdb file.
pdb_structure_pdb_uri :: String -> String
pdb_structure_pdb_uri k = "https://files.rcsb.org/download/" ++ k ++ ".pdb"

-- | Uri for structure FASTA file, accepts 4-letter Pdb code.
pdb_structure_fasta_uri :: String -> String
pdb_structure_fasta_uri = (++) "https://www.rcsb.org/fasta/entry/"

-- | Uri for ligand summary.
pdb_ligand_summary_uri :: String -> String
pdb_ligand_summary_uri = (++) "http://www.rcsb.org/ligand/"

{- | Uri for ligand Sdf file.

>>> pdb_ligand_sdf_uri "ideal" "ALA"
"http://files.rcsb.org/ligands/view/ALA_ideal.sdf"

>>> pdb_ligand_sdf_uri "model" "ALA"
"http://files.rcsb.org/ligands/view/ALA_model.sdf"
-}
pdb_ligand_sdf_uri :: String -> String -> String
pdb_ligand_sdf_uri ty k = "http://files.rcsb.org/ligands/view/" ++ pdb_ligand_sdf_filename ty k

{-
https://pdb101.rcsb.org/learn/guide-to-understanding-pdb-data/small-molecule-ligands
-}

-- * Amino Acid Tables

{- | Kyte, J; Doolittle, R. F. (1982).
  "A simple method for displaying the hydropathic character of a protein".
  Journal of Molecular Biology. 157 (1): 105–32
-}
amino_acid_hydropathy_tbl :: [(String, Char, Double)]
amino_acid_hydropathy_tbl =
  [ ("Isoleucine", 'I', 4.5)
  , ("Valine", 'V', 4.2)
  , ("Leucine", 'L', 3.8)
  , ("Phenylalanine", 'F', 2.8)
  , ("Cysteine", 'C', 2.5)
  , ("Methionine", 'M', 1.9)
  , ("Alanine", 'A', 1.8)
  , ("Glycine", 'G', -0.4)
  , ("Threonine", 'T', -0.7)
  , ("Serine", 'S', -0.8)
  , ("Tryptophan", 'W', -0.9)
  , ("Tyrosine", 'Y', -1.3)
  , ("Proline", 'P', -1.6)
  , ("Histidine", 'H', -3.2)
  , ("Glutamic Acid", 'E', -3.5)
  , ("Glutamine", 'Q', -3.5)
  , ("Aspartic Acid", 'D', -3.5)
  , ("Asparagine", 'N', -3.5)
  , ("Lysine", 'K', -3.9)
  , ("Arginine", 'R', -4.5)
  ]

{- | <http://education.expasy.org/student_projects/isotopident/htdocs/aa-list.html>

>>> length amino_acid_monoisotopic_mass_tbl
20
-}
amino_acid_monoisotopic_mass_tbl :: [(String, Double)]
amino_acid_monoisotopic_mass_tbl =
  [ ("Gly", 57.021464)
  , ("Ala", 71.037114)
  , ("Ser", 87.032029)
  , ("Pro", 97.052764)
  , ("Val", 99.068414)
  , ("Thr", 101.04768)
  , ("Cys", 103.00919)
  , ("Asn", 114.04293)
  , ("Asp", 115.02694)
  , ("Leu", 113.08406)
  , ("Ile", 113.08406)
  , ("Gln", 128.05858)
  , ("Lys", 128.09496)
  , ("Glu", 129.04259)
  , ("Met", 131.04048)
  , ("His", 137.05891)
  , ("Phe", 147.06841)
  , ("Arg", 156.10111)
  , ("Tyr", 163.06333)
  , ("Trp", 186.07931)
  ]

{- | The masses listed (in daltons) are based on weighted averages of the elemental isotopes at their natural abundances.

>>> length amino_acid_average_mass_table
22
-}
amino_acid_average_mass_table :: [(String, Double)]
amino_acid_average_mass_table =
  [ ("Ala", 89.09404)
  , ("Cys", 121.15404)
  , ("Asp", 133.10384)
  , ("Glu", 147.13074)
  , ("Phe", 165.19184)
  , ("Gly", 75.06714)
  , ("His", 155.15634)
  , ("Ile", 131.17464)
  , ("Lys", 146.18934)
  , ("Leu", 131.17464)
  , ("Met", 149.20784)
  , ("Asn", 132.11904)
  , ("Pyl", 255.31)
  , ("Pro", 115.13194)
  , ("Gln", 146.14594)
  , ("Arg", 174.20274)
  , ("Ser", 105.09344)
  , ("Thr", 119.12034)
  , ("Sec", 168.053)
  , ("Val", 117.14784)
  , ("Trp", 204.22844)
  , ("Tyr", 181.19124)
  ]

-- * Io

-- | Load Pdb file as Pdb.
pdb_load :: FilePath -> IO Pdb.Types.Pdb
pdb_load = fmap Pdb.Parse.dat_parse . Pdb.Parse.pdb_load_dat

-- | Load directory of Pdb files as list of Pdb.
pdb_load_dir :: FilePath -> IO [Pdb.Types.Pdb]
pdb_load_dir = fmap (map Pdb.Parse.dat_parse) . Pdb.Parse.pdb_load_dat_dir
