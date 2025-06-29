{- | Pdb = Protein Data Bank = <http://www.wwpdb.org/>

Research Collaboratory for Structural Bioinformatics Pdb = <https://www.rcsb.org/>
Pdbj (Protein Data Bank Japan) = <https://pdbj.org/>
Pdbe (Protein Data Bank in Europe) = <https://www.ebi.ac.uk/pdbe/>
Biological Magnetic Resonance Data Bank = <http://www.bmrb.wisc.edu/>
-}
module Sound.Sc3.Data.Chemistry.Pdb where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}
import System.Process {- process -}

import qualified Data.ByteString.Char8 as ByteString.Char8 {- bytestring -}

import qualified Music.Theory.Directory as Directory {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}

import qualified Sound.Sc3.Data.Chemistry.Elements as Elements {- hsc3-data -}
import qualified Sound.Sc3.Data.Chemistry.Iupac as Iupac {- hsc3-data -}
import qualified Sound.Sc3.Data.Chemistry.Pdb.Parse as Pdb.Parse {- hsc3-data -}
import qualified Sound.Sc3.Data.Chemistry.Pdb.Types as Pdb.Types {- hsc3-data -}

-- * Amino Acid and Nucleotide Nomenclature

-- | The standard Pdb codes for amino acids, deoxyribonucleotides and ribonucleotides.
pdb_std_codes :: ([String], [String], [String])
pdb_std_codes =
  ( words "ALA CYS ASP GLU PHE GLY HIS ILE LYS LEU MET ASN PRO GLN ARG SER THR VAL TRP TYR"
  , words "DA DC DG DT DI"
  , words "A C G U I"
  )

{- | Is /x/ a standard Pdb code?

> all pdb_is_std_code (words "LEU DT U") == True
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

pdb_nucleotides :: [String]
pdb_nucleotides = let (_, d, r) = pdb_std_codes in d ++ r

-- | (Pdb-Code,Iupac-Code)
pdb_code_tbl :: [(String, Char)]
pdb_code_tbl =
  concat
    [ map (\(c1, c3, _) -> (map toUpper c3, c1)) Iupac.iupac_amino_acid_tbl
    , map (\x -> (x, last x)) pdb_nucleotides
    ]

-- | Translate Pdb SEQRES code (upper case 3-letter code) to IUPAC code.
pdb_seqres_code_lookup :: String -> Maybe Char
pdb_seqres_code_lookup = flip lookup pdb_code_tbl

{- | Erroring variant.

>>> pdb_seqres_code_lookup_err "GLY"
'G'

>>> map pdb_seqres_code_lookup_err pdb_amino_acids
"ACDEFGHIKLMNPQRSTVWY"
-}
pdb_seqres_code_lookup_err :: String -> Char
pdb_seqres_code_lookup_err = fromMaybe (error "pdb_seqres_code_lookup?") . pdb_seqres_code_lookup

-- * Convert

{- | Run obabel process to convert Pdb file to Mol file.

Pdb files are converted to Mol files using obabel,
<https://packages.debian.org/stable/openbabel>
-}
pdb_to_mol :: FilePath -> FilePath -> IO ()
pdb_to_mol pdb_fn mol_fn = callProcess "obabel" ["-ipdb", pdb_fn, "-omol", "-O", mol_fn]

-- | Variant that only runs if the Mol file does not already exist.
pdb_to_mol_x :: FilePath -> FilePath -> IO ()
pdb_to_mol_x pdb_fn mol_fn = do
  createDirectoryIfMissing True (takeDirectory mol_fn)
  Directory.if_file_exists (mol_fn, return (), pdb_to_mol pdb_fn mol_fn)

-- * Monomer-Het

-- | Uri for het_dictionary (52,082,480 BYTES)
het_dictionary_uri :: String
het_dictionary_uri = "ftp://ftp.wwpdb.org/pub/pdb/data/monomers/het_dictionary.txt"

{- | Uri for monomer RESIDUE file.

>>> het_residue_uri "GLY"
"ftp://ftp.wwpdb.org/pub/pdb/data/monomers/GLY"
-}
het_residue_uri :: String -> String
het_residue_uri = (++) "ftp://ftp.wwpdb.org/pub/pdb/data/monomers/"

{- | Uri for monomer CIF file.

>>> het_cif_uri "GLY"
"https://files.rcsb.org/ligands/download/GLY.cif"
-}
het_cif_uri :: String -> String
het_cif_uri k = "https://files.rcsb.org/ligands/download/" ++ k ++ ".cif"

-- | Type for RECORD in 'het_dictionary'
type Het_Record = [ByteString.Char8.ByteString]

-- | Get (NAME,N-ATOMS) for residue at record.
het_parse_residue :: Het_Record -> (String, Int)
het_parse_residue r =
  case r of
    e : _ -> case words (ByteString.Char8.unpack e) of
      ["RESIDUE", nm, sz] -> (nm, read sz)
      x -> error (show ("het_parse_residue", x))
    _ -> error (show ("het_parse_residue", r))

-- | Select fields of type /k/ at record.
het_field_sel :: String -> Het_Record -> [ByteString.Char8.ByteString]
het_field_sel k = filter (ByteString.Char8.isPrefixOf (ByteString.Char8.pack k))

-- | Parse CONECT fields at record, which are of the form (lhs,[rhs])
het_parse_conect :: Het_Record -> [(String, [String])]
het_parse_conect r =
  let f s = case words (ByteString.Char8.unpack s) of
        "CONECT" : lhs : cnt : rhs ->
          if length rhs == read cnt
            then (lhs, rhs)
            else error (show ("het_parse_conect", lhs, cnt, rhs))
        x -> error (show ("het_parse_conect", x))
  in map f (het_field_sel "CONECT" r)

-- | Parse HETNAM field at record.
het_parse_hetnam :: Het_Record -> String
het_parse_hetnam = unwords . map (ByteString.Char8.unpack . ByteString.Char8.drop 15) . het_field_sel "HETNAM"

-- | Parse FORMUL field at record.
het_parse_formul :: Het_Record -> String
het_parse_formul = unwords . map (ByteString.Char8.unpack . ByteString.Char8.drop 19) . het_field_sel "FORMUL"

-- | Convert CONECT fields to edge set.
het_edge_set :: [(String, [String])] -> [(String, String)]
het_edge_set =
  let f (lhs, rhs) = zip (repeat lhs) rhs
      g (i, j) = (min i j, max i j)
  in map g . concatMap f

-- | Convert CONECT fields to vertex set.
het_vertex_set :: [(String, [String])] -> [String]
het_vertex_set = let f (lhs, rhs) = lhs : rhs in nub . sort . concatMap f

-- | Load records from local copy of 'het_dictionary'.
het_load_records :: FilePath -> IO [Het_Record]
het_load_records fn = do
  s <- ByteString.Char8.readFile fn
  let l = ByteString.Char8.lines s
      r = List.split_when_keeping_left (ByteString.Char8.isPrefixOf (ByteString.Char8.pack "RESIDUE")) l
  return (filter (not . null) r)

-- | ((ID3,N-ATOMS),NAME,FORMUL,GRAPH)
type Het_Entry = ((String, Int), String, String, ([String], [(String, String)]))

-- | ID3 field.
het_entry_id3 :: Het_Entry -> String
het_entry_id3 ((nm, _), _, _, _) = nm

-- | N-ATOMS field.
het_entry_n_atoms :: Het_Entry -> Int
het_entry_n_atoms ((_, k), _, _, _) = k

-- | FORMULA field.
het_entry_formula :: Het_Entry -> String
het_entry_formula (_, _, x, _) = x

-- | Parse record to entry.
het_parse_entry :: Het_Record -> Het_Entry
het_parse_entry r =
  let c = het_parse_conect r
  in ( het_parse_residue r
     , het_parse_hetnam r
     , het_parse_formul r
     , (het_vertex_set c, het_edge_set c)
     )

-- | Lookup Het_Entry by name.
het_entry_lookup :: String -> [Het_Entry] -> Maybe Het_Entry
het_entry_lookup k = find (\((nm, _), _, _, _) -> nm == k)

{- | Load Het_Entry from local copy of 'het_dictionary'.

>>> fn = "/home/rohan/data/pdb/monomers/het_dictionary.txt"
>>> e <- het_load_entries fn
>>> length e
31253

>>> het_entry_lookup "GLY" e
Just (("GLY",10),"GLYCINE","C2 H5 N1 O2",(["C","CA","H","H2","HA2","HA3","HXT","N","O","OXT"],[("CA","N"),("H","N"),("H2","N"),("CA","N"),("C","CA"),("CA","HA2"),("CA","HA3"),("C","CA"),("C","O"),("C","OXT"),("C","O"),("C","OXT"),("HXT","OXT"),("H","N"),("H2","N"),("CA","HA2"),("CA","HA3"),("HXT","OXT")]))

> map (flip het_entry_lookup e . map toUpper . \(_,x,_) -> x) proteinogenic_amino_acid_tbl
-}
het_load_entries :: FilePath -> IO [Het_Entry]
het_load_entries = fmap (map het_parse_entry) . het_load_records

-- | Histogram of elememts derived from FORMULA field.
het_entry_formula_hist :: Het_Entry -> [(String, Int)]
het_entry_formula_hist = sort . fst . Elements.formula_ch_parse . het_entry_formula

-- | Does the N-ATOMS field correlate with the FORMULA field?
het_entry_formula_validate :: Het_Entry -> Bool
het_entry_formula_validate e =
  let k = sum (map snd (het_entry_formula_hist e))
  in k == het_entry_n_atoms e

-- * File-Names

{- | Pdb filenames are lower case, with a .pdb extension. Identifiers are upper-case.

>>> pdb_file_name_to_id "rscb/1poc.pdb"
"1POC"
-}
pdb_file_name_to_id :: FilePath -> String
pdb_file_name_to_id = map toUpper . dropExtension . takeFileName

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

-- | <http://education.expasy.org/student_projects/isotopident/htdocs/aa-list.html>
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

-- * Io

-- | Load Pdb file as Pdb.
pdb_load :: FilePath -> IO Pdb.Types.Pdb
pdb_load = fmap Pdb.Parse.dat_parse . Pdb.Parse.pdb_load_dat

-- | Load directory of Pdb files as list of Pdb.
pdb_load_dir :: FilePath -> IO [Pdb.Types.Pdb]
pdb_load_dir = fmap (map Pdb.Parse.dat_parse) . Pdb.Parse.pdb_load_dat_dir
