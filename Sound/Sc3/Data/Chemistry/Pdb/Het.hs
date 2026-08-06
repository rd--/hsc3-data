-- | Pdb Monomer-Het
module Sound.Sc3.Data.Chemistry.Pdb.Het where

import qualified Data.List {- base -}

import qualified Data.ByteString.Char8 as ByteString.Char8 {- bytestring -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Sound.Sc3.Data.Chemistry.Elements as Elements {- hsc3-data -}

-- | Uri for het_dictionary (Ftp)
het_dictionary_uri_ftp :: String
het_dictionary_uri_ftp = "ftp://ftp.wwpdb.org/pub/pdb/data/monomers/het_dictionary.txt"

{- | Uri for het_dictionary (87,380,975 bytes, 2026-08-04)

<https://www.wwpdb.org/data/ccd>
-}
het_dictionary_uri :: String
het_dictionary_uri = "https://files.wwpdb.org/pub/pdb/data/monomers/het_dictionary.txt"

{- | Uri for monomer Residue file (invidual entry from het dictionary).

>>> het_residue_uri_ftp "GLY"
"ftp://ftp.wwpdb.org/pub/pdb/data/monomers/GLY"
-}
het_residue_uri_ftp :: String -> String
het_residue_uri_ftp = (++) "ftp://ftp.wwpdb.org/pub/pdb/data/monomers/"

{- | Uri for invidual entry from het dictionary

>>> het_residue_uri "ATP"
"https://files.wwpdb.org/pub/pdb/data/monomers/ATP"
-}
het_residue_uri :: String -> String
het_residue_uri nm = "https://files.wwpdb.org/pub/pdb/data/monomers/" ++ nm

{- | Uri for monomer Cif file.

>>> het_cif_uri "GLY"
"https://files.rcsb.org/ligands/download/GLY.cif"
-}
het_cif_uri :: String -> String
het_cif_uri k = "https://files.rcsb.org/ligands/download/" ++ k ++ ".cif"

-- | Type for each record in 'het_dictionary'
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

-- | Type for CONECT record in HET file.
type Het_Conect = (String, Int, [String])

-- | Parse CONECT fields at record, which are of the form (lhs,[rhs])
het_parse_conect :: Het_Record -> [Het_Conect]
het_parse_conect r =
  let f s = case words (ByteString.Char8.unpack s) of
        "CONECT" : lhs : cnt : rhs ->
          if length rhs == read cnt
            then (lhs, read cnt, rhs)
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
het_edge_set :: [Het_Conect] -> [(String, String)]
het_edge_set =
  let f (lhs, _cnt, rhs) = zip (repeat lhs) rhs
      g (i, j) = (min i j, max i j)
  in map g . concatMap f

-- | Convert CONECT fields to vertex set.
het_vertex_set :: [Het_Conect] -> [String]
het_vertex_set = let f (lhs, _cnt, rhs) = lhs : rhs in List.nub_sort . concatMap f

-- | Load records from local copy of 'het_dictionary'.
het_load_records :: FilePath -> IO [Het_Record]
het_load_records fn = do
  s <- ByteString.Char8.readFile fn
  let l = ByteString.Char8.lines s
      r = List.split_when_keeping_left (ByteString.Char8.isPrefixOf (ByteString.Char8.pack "RESIDUE")) l
  return (filter (not . null) r)

-- | ((Id3,N-Atoms),Name,Formul,Graph)
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
het_entry_lookup k = Data.List.find (\((nm, _), _, _, _) -> nm == k)

{- | Load Het_Entry from local copy of 'het_dictionary'.

>>> fn = "/home/rohan/data/pdb/monomers/het_dictionary.txt"
>>> e <- het_load_entries fn
>>> length e
50782

>>> het_entry_lookup "GLY" e
Just (("GLY",10),"GLYCINE","C2 H5 N O2",(["C","CA","H","H2","HA2","HA3","HXT","N","O","OXT"],[("CA","N"),("H","N"),("H2","N"),("CA","N"),("C","CA"),("CA","HA2"),("CA","HA3"),("C","CA"),("C","O"),("C","OXT"),("C","O"),("C","OXT"),("HXT","OXT"),("H","N"),("H2","N"),("CA","HA2"),("CA","HA3"),("HXT","OXT")]))

> map (flip het_entry_lookup e . map Data.Char.toUpper . \(x,_) -> x) amino_acid_average_mass_table
-}
het_load_entries :: FilePath -> IO [Het_Entry]
het_load_entries = fmap (map het_parse_entry) . het_load_records

-- | Histogram of elememts derived from FORMULA field.
het_entry_formula_hist :: Het_Entry -> [(String, Int)]
het_entry_formula_hist = Data.List.sort . fst . Elements.formula_ch_parse . het_entry_formula

-- | Does the N-ATOMS field correlate with the FORMULA field?
het_entry_formula_validate :: Het_Entry -> Bool
het_entry_formula_validate e =
  let k = sum (map snd (het_entry_formula_hist e))
  in k == het_entry_n_atoms e

