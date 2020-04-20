{- | PDB = Protein Data Bank = <http://www.wwpdb.org/>

Research Collaboratory for Structural Bioinformatics PDB = <https://www.rcsb.org/>
PDBj (Protein Data Bank Japan) = <https://pdbj.org/>
PDBe (Protein Data Bank in Europe) = <https://www.ebi.ac.uk/pdbe/>
Biological Magnetic Resonance Data Bank = <http://www.bmrb.wisc.edu/>
-}
module Sound.SC3.Data.Chemistry.PDB where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}
import System.Process {- process -}

import qualified Data.ByteString.Char8 as B {- bytestring -}

import qualified Music.Theory.Directory as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}

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

-- | Lookup IUPAC code in 'proteinogenic_amino_acid_tbl'.
iupac_code_to_three_letter_code :: Char -> Maybe String
iupac_code_to_three_letter_code x =
  let f (c1,_,_) = x == c1
      g (_,c3,_) = c3
  in fmap g (find f proteinogenic_amino_acid_tbl)

-- | (IUPAC-CODE,DESCRIPTION,COMPLEMENT)
nucleotide_core :: [(Char,String,Char)]
nucleotide_core =
  [('A',"Adenine",'T')
  ,('C',"Cytosine",'G')
  ,('G',"Guanine",'C')
  ,('T',"Thymine",'A')
  ,('U',"Uracil",'A')] -- IN RNA URACIL IS USED IN PLACE OF THYMINE

-- | (IUPAC-CODE,DESCRIPTION,COMPLEMENT)
--
-- <https://www.bioinformatics.org/sms/iupac.html>
nucleotide_iupac :: [(Char, String, Char)]
nucleotide_iupac =
  nucleotide_core ++
  [('W',"Weak",'W')
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

-- | (PDB-CODE,IUPAC-CODE)
pdb_code_tbl :: [(String,Char)]
pdb_code_tbl =
  map (\(c1,c3,_) -> (map toUpper c3,c1)) proteinogenic_amino_acid_tbl ++
  [("DA",'A')
  ,("DC",'C')
  ,("DG",'G')
  ,("DT",'T')]

{- | Translate PDB SEQRES code to IUPAC code.

> pdb_seqres_code_lookup "GLY" == Just 'G'
-}
pdb_seqres_code_lookup :: String -> Maybe Char
pdb_seqres_code_lookup = flip lookup pdb_code_tbl

{- | Erroring variant.

> pdb_seqres_code_lookup_err "GLY" == 'G'

> let s = "ALA CYS ASP GLU PHE GLY HIS ILE LYS LEU MET ASN PRO GLN ARG SER THR VAL TRP TYR"
> map pdb_seqres_code_lookup_err (words s) == "ACDEFGHIKLMNPQRSTVWY"

> let s = "DA DC DG DT"
> map pdb_seqres_code_lookup_err (words s) == "ACGT"
-}
pdb_seqres_code_lookup_err :: String -> Char
pdb_seqres_code_lookup_err = fromMaybe (error "pdb_seqres_code_lookup?") . pdb_seqres_code_lookup

-- * CONVERT

{- | Run obabel process to convert PDB file to MOL file.

PDB files are converted to MOL files using obabel,
<https://packages.debian.org/stable/openbabel>
-}
pdb_to_mol :: FilePath -> FilePath -> IO ()
pdb_to_mol pdb_fn mol_fn = callProcess "obabel" [pdb_fn,"-O",mol_fn]

-- | Variant that only runs if the MOL file does not already exist.
pdb_to_mol_x :: FilePath -> FilePath -> IO ()
pdb_to_mol_x pdb_fn mol_fn = do
  createDirectoryIfMissing True (takeDirectory mol_fn)
  T.if_file_exists (mol_fn,pdb_to_mol pdb_fn mol_fn,return ())

-- * MONOMER-HET

-- | URI for het_dictionary (52,082,480 BYTES)
het_dictionary_uri :: String
het_dictionary_uri = "ftp://ftp.wwpdb.org/pub/pdb/data/monomers/het_dictionary.txt"

-- | URI for monomer RESIDUE file.
--
-- > het_residue_uri "GLY" == "ftp://ftp.wwpdb.org/pub/pdb/data/monomers/GLY"
het_residue_uri :: String -> String
het_residue_uri = (++) "ftp://ftp.wwpdb.org/pub/pdb/data/monomers/"

-- | URI for monomer CIF file.
--
-- > het_cif_uri "GLY" == "https://files.rcsb.org/ligands/download/GLY.cif"
het_cif_uri :: String -> String
het_cif_uri k = "https://files.rcsb.org/ligands/download/" ++ k ++ ".cif"

-- | Type for RECORD in 'het_dictionary'
type HET_RECORD = [B.ByteString]

-- | Get (NAME,N-ATOMS) for residue at record.
het_parse_residue :: HET_RECORD -> (String,Int)
het_parse_residue r =
  case r of
    e:_ -> case words (B.unpack e) of
             ["RESIDUE",nm,sz] -> (nm,read sz)
             x -> error (show ("het_parse_residue",x))
    _ -> error (show ("het_parse_residue",r))

-- | Select fields of type /k/ at record.
het_field_sel :: String -> HET_RECORD -> [B.ByteString]
het_field_sel k = filter (B.isPrefixOf (B.pack k))

-- | Parse CONECT fields at record, which are of the form (lhs,[rhs])
het_parse_conect :: HET_RECORD -> [(String,[String])]
het_parse_conect r =
  let f s = case words (B.unpack s) of
              "CONECT":lhs:cnt:rhs -> if length rhs == read cnt
                                      then (lhs,rhs)
                                      else error (show ("het_parse_conect",lhs,cnt,rhs))
              x -> error (show ("het_parse_conect",x))
  in map f (het_field_sel "CONECT" r)

-- | Parse HETNAM field at record.
het_parse_hetnam :: HET_RECORD -> String
het_parse_hetnam = unwords . map (B.unpack . B.drop 15) . het_field_sel "HETNAM"

-- | Parse FORMUL field at record.
het_parse_formul :: HET_RECORD -> String
het_parse_formul = unwords . map (B.unpack . B.drop 19) . het_field_sel "FORMUL"

-- | Convert CONECT fields to edge set.
het_edge_set :: [(String,[String])] -> [(String,String)]
het_edge_set =
  let f (lhs,rhs) = zip (repeat lhs) rhs
      g (i,j) = (min i j,max i j)
  in map g . concatMap f

-- | Convert CONECT fields to vertex set.
het_vertex_set :: [(String,[String])] -> [String]
het_vertex_set = let f (lhs,rhs) = lhs : rhs in nub . sort . concatMap f

-- | Load records from local copy of 'het_dictionary'.
het_load_records :: FilePath -> IO [HET_RECORD]
het_load_records fn = do
  s <- B.readFile fn
  let l = B.lines s
      r = T.split_when_keeping_left (B.isPrefixOf (B.pack "RESIDUE")) l
  return (filter (not . null) r)

-- | ((ID3,N-ATOMS),NAME,FORMUL,GRAPH)
type HET_ENTRY = ((String,Int),String,String, ([String], [(String, String)]))

-- | Parse record to entry.
het_parse_entry :: HET_RECORD -> HET_ENTRY
het_parse_entry r =
  let c = het_parse_conect r
  in (het_parse_residue r
     ,het_parse_hetnam r
     ,het_parse_formul r
     ,(het_vertex_set c,het_edge_set c))

-- | Lookup HET_ENTRY by name.
het_entry_lookup :: String -> [HET_ENTRY] -> Maybe HET_ENTRY
het_entry_lookup k = find (\((nm,_),_,_,_) -> nm == k)

{- | Load HET_ENTRY from local copy of 'het_dictionary'.

> fn = "/home/rohan/data/pdb/monomers/het_dictionary.txt"
> e <- het_load_entries fn
> length e == 31253
> het_entry_lookup "GLY" e
> map (flip het_entry_lookup e . map toUpper . \(_,x,_) -> x) proteinogenic_amino_acid_tbl
-}
het_load_entries :: FilePath -> IO [HET_ENTRY]
het_load_entries = fmap (map het_parse_entry) . het_load_records

-- * FILE-NAMES

-- | PDB filenames are lower case, with a .pdb extension. Identifiers are upper-case.
--
-- > pdb_file_name_to_id "rscb/1poc.pdb" == "1POC"
pdb_file_name_to_id :: FilePath -> String
pdb_file_name_to_id = map toUpper . dropExtension . takeFileName

-- | Filename for ligand /k/, /ty/ is "ideal" or "model"
--
-- > pdb_ligand_sdf_filename "ideal" "GLY" == "GLY_ideal.sdf"
pdb_ligand_sdf_filename :: String -> String -> String
pdb_ligand_sdf_filename ty k = concat [k,"_",ty,".sdf"]

-- * RCSB-URI

-- | URI for structure summary.
pdb_structure_summary_uri :: String -> String
pdb_structure_summary_uri = (++) "http://www.rcsb.org/structure/"

-- | URI for structure PDB file.
pdb_structure_pdb_uri :: String -> String
pdb_structure_pdb_uri k = "https://files.rcsb.org/download/" ++ k ++ ".pdb"

-- | URI for structure FASTA file.
pdb_structure_fasta_uri :: String -> String
pdb_structure_fasta_uri = (++) "https://www.rcsb.org/fasta/entry/"

-- | URI for ligand summary.
pdb_ligand_summary_uri :: String -> String
pdb_ligand_summary_uri = (++) "http://www.rcsb.org/ligand/"

-- | URI for ligand SDF file.
--
-- > pdb_ligand_sdf_uri "ideal" "ALA" == "http://files.rcsb.org/ligands/view/ALA_ideal.sdf"
pdb_ligand_sdf_uri :: String -> String -> String
pdb_ligand_sdf_uri ty k = "http://files.rcsb.org/ligands/view/" ++ pdb_ligand_sdf_filename ty k

{-
https://pdb101.rcsb.org/learn/guide-to-understanding-pdb-data/small-molecule-ligands
-}
