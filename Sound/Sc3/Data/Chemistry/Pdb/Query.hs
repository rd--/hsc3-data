module Sound.Sc3.Data.Chemistry.Pdb.Query where

import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import Music.Theory.Geometry.Vector (V3) {- hmt-base -}

import qualified Sound.Sc3.Data.Chemistry.Pdb.Parse as Parse {- hsc3-data -}
import Sound.Sc3.Data.Chemistry.Pdb.Types {- hsc3-data -}

-- * Stat

pdb_stat :: Pdb -> [(String, String)]
pdb_stat ((h1, h2, h3), t, m, _, (a, h), c, sq, hlx, sht, lnk, ssb) =
  let e = nub (sort (map atom_element (a ++ h)))
      uniq_ch = map fst . nubBy ((==) `on` snd)
      u = uniq_ch sq
      alt = map atom_altloc (a ++ h)
      res = nub (sort (map atom_residue_id (a ++ h)))
      hoh = filter (\(nm, _, _, _) -> nm == "HOH") res
  in [ ("ID", h3)
     , ("CLASSIFICATION", h1)
     , ("DEPOSITION-DATE", h2)
     , ("TITLE", t)
     , ("NUMMDL", show (fromMaybe 1 m))
     ]
      ++ if isNothing m
        then
          [ ("N-ATOM", show (length a))
          , ("N-HETATM", show (length h))
          , ("ATOM-ALT-ID", altloc_id_set alt)
          , ("N-ATOM-ALT", show (length (filter (/= ' ') alt)))
          , ("N-CHAIN", show (length sq))
          , ("N-UNIQ-CHAIN", show (length u))
          , ("CHAIN-ID-SEQ", map fst sq)
          , ("UNIQ-CHAIN-ID-SEQ", u)
          , ("N-ELEMENTS", show (length e))
          , ("ELEMENTS", unwords e)
          , ("N-CONECT", show (length c))
          , ("N-LINK", show (length lnk))
          , ("N-SSBOND", show (length ssb))
          , ("N-HELIX", show (length hlx))
          , ("N-SHEET", show (length sht))
          , ("SEQRES-N", show (sum (map (length . snd) sq)))
          , ("RES-N", show (length res))
          , ("HOH-N", show (length hoh))
          ]
        else []

dat_stat :: Parse.Dat -> [(String, String)]
dat_stat = pdb_stat . Parse.dat_parse

-- * Alpha Carbon

{- | Generate Cα chains of single model Pdb.
     Atoms where Altloc is not ' ' or 'A' are deleted.
     Atoms that are located past a Ter record are deleted.
     Nucleotide chains are not given as null entries.
-}
dat_to_alpha_carbon_chains :: Bool -> Parse.Dat -> Maybe [(Char, [V3 Double])]
dat_to_alpha_carbon_chains uniq dat =
  if isJust (Parse.dat_nummdl dat)
    then Nothing
    else
      let t = Parse.dat_ter dat
          a = map (atom_apply_ter t) (atom_group (filter atom_sel_altloc_A (Parse.dat_atom_all dat)))
          uniq_ch = map fst . nubBy ((==) `on` snd)
          u = uniq_ch (seqres_group (Parse.dat_seqres dat))
          c = if uniq then filter (flip elem u . fst) a else a
          p = map (map atom_coord . filter ((==) "CA" . atom_name) . snd) c
      in Just (filter (not . null . snd) (zip (map fst c) p))

dat_to_alpha_carbon_chains_err :: Bool -> Parse.Dat -> [(Char, [V3 Double])]
dat_to_alpha_carbon_chains_err uniq = fromMaybe (error "dat_to_alpha_carbon_chains") . dat_to_alpha_carbon_chains uniq

-- * Residues

-- | Set of all residue names at Atom records.
atom_residue_set :: Parse.Dat -> [String]
atom_residue_set = nub . sort . map (residue_id_name . atom_residue_id) . Parse.dat_atom__

-- | Set of all residue names at Hetatm records.
hetatm_residue_set :: Parse.Dat -> [String]
hetatm_residue_set = nub . sort . map (residue_id_name . atom_residue_id) . Parse.dat_hetatm

-- | Set of all residue names at Seqres records.
seqres_residue_set :: Parse.Dat -> [String]
seqres_residue_set = nub . sort . concatMap seqres_residue_names . Parse.dat_seqres

-- | Set of all residue names at Modres records.
modres_residue_set :: Parse.Dat -> [String]
modres_residue_set = nub . sort . concatMap ((\(i, j) -> [i, j]) . modres_names) . Parse.dat_modres

-- | Residue sets (Atom,Hetatm,Seqres,Modres).
residue_sets :: Parse.Dat -> ([String], [String], [String], [String])
residue_sets x = (atom_residue_set x, hetatm_residue_set x, seqres_residue_set x, modres_residue_set x)

-- | Set of 'residue_sets'
residue_sets_concat :: Parse.Dat -> [String]
residue_sets_concat = nub . sort . concat . (\(a, b, c, d) -> [a, b, c, d]) . residue_sets

-- | Atom/residue stat for selector predicate.
atom_residue_stat_of :: (Atom -> Bool) -> FilePath -> IO ()
atom_residue_stat_of predicate pdb_fn = do
  d <- Parse.pdb_load_dat pdb_fn
  let a = filter predicate (filter atom_sel_altloc_A (Parse.dat_atom_all d))
  print ("N-ATOM", length a)
  print ("N-RESIDUES", length (nub (sort (map atom_residue_id a))))

-- * Water

is_water :: Atom -> Bool
is_water = (==) "HOH" . residue_id_name . atom_residue_id
