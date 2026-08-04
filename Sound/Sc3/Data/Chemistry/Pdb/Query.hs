-- | Pdb (Protein Data Bank) Query Functions
module Sound.Sc3.Data.Chemistry.Pdb.Query where

import qualified Data.Function {- base -}
import qualified Data.List {- base -}
import qualified Data.Maybe {- base -}

import qualified Music.Theory.Geometry.Vector as Vector {- hmt-base -}

import qualified Sound.Sc3.Data.Chemistry.Pdb.Parse as Pdb.Parse {- hsc3-data -}
import qualified Sound.Sc3.Data.Chemistry.Pdb.Types as Pdb {- hsc3-data -}

-- * Stat

-- | Answer a list of (key,value) pairs giving an overview of the given Pdb structure.
pdb_stat :: Pdb.Pdb -> [(String, String)]
pdb_stat ((h1, h2, h3), t, m, _, (a, h), c, sq, hlx, sht, lnk, ssb) =
  let e = Data.List.nub (Data.List.sort (map Pdb.atom_element (a ++ h)))
      uniq_ch = map fst . Data.List.nubBy ((==) `Data.Function.on` snd)
      u = uniq_ch sq
      alt = map Pdb.atom_altloc (a ++ h)
      res = Data.List.nub (Data.List.sort (map Pdb.atom_residue_id (a ++ h)))
      hoh = filter (\(nm, _, _, _) -> nm == "HOH") res
  in [ ("ID", h3)
     , ("CLASSIFICATION", h1)
     , ("DEPOSITION-DATE", h2)
     , ("TITLE", t)
     , ("NUMMDL", show (Data.Maybe.fromMaybe 1 m))
     ]
      ++ if Data.Maybe.isNothing m
        then
          [ ("N-ATOM", show (length a))
          , ("N-HETATM", show (length h))
          , ("ATOM-ALT-ID", Pdb.altloc_id_set alt)
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

dat_stat :: Pdb.Parse.Dat -> [(String, String)]
dat_stat = pdb_stat . Pdb.Parse.dat_parse

-- * Alpha Carbon

{- | Generate Cα chains of single model Pdb.
     Atoms where Altloc is not ' ' or 'A' are deleted.
     Atoms that are located past a Ter record are deleted.
     Nucleotide chains are not given as null entries.
-}
dat_to_alpha_carbon_chains :: Bool -> Pdb.Parse.Dat -> Maybe [(Char, [Vector.V3 Double])]
dat_to_alpha_carbon_chains uniq dat =
  if Data.Maybe.isJust (Pdb.Parse.dat_nummdl dat)
    then Nothing
    else
      let t = Pdb.Parse.dat_ter dat
          a = map (Pdb.atom_apply_ter t) (Pdb.atom_group (filter Pdb.atom_sel_altloc_A (Pdb.Parse.dat_atom_all dat)))
          uniq_ch = map fst . Data.List.nubBy ((==) `Data.Function.on` snd)
          u = uniq_ch (Pdb.seqres_group (Pdb.Parse.dat_seqres dat))
          c = if uniq then filter (flip elem u . fst) a else a
          p = map (map Pdb.atom_coord . filter ((==) "CA" . Pdb.atom_name) . snd) c
      in Just (filter (not . null . snd) (zip (map fst c) p))

dat_to_alpha_carbon_chains_err :: Bool -> Pdb.Parse.Dat -> [(Char, [Vector.V3 Double])]
dat_to_alpha_carbon_chains_err uniq =
  Data.Maybe.fromMaybe (error "dat_to_alpha_carbon_chains")
  . dat_to_alpha_carbon_chains uniq

-- * Residues

-- | Set of all residue names at Atom records.
atom_residue_set :: Pdb.Parse.Dat -> [String]
atom_residue_set =
  Data.List.nub .
  Data.List.sort .
  map (Pdb.residue_id_name . Pdb.atom_residue_id) .
  Pdb.Parse.dat_atom__

-- | Set of all residue names at Hetatm records.
hetatm_residue_set :: Pdb.Parse.Dat -> [String]
hetatm_residue_set =
  Data.List.nub
  . Data.List.sort
  . Data.List.map (Pdb.residue_id_name . Pdb.atom_residue_id)
  . Pdb.Parse.dat_hetatm

-- | Set of all residue names at Seqres records.
seqres_residue_set :: Pdb.Parse.Dat -> [String]
seqres_residue_set =
  Data.List.nub
  . Data.List.sort
  . concatMap Pdb.seqres_residue_names
  . Pdb.Parse.dat_seqres

-- | Set of all residue names at Modres records.
modres_residue_set :: Pdb.Parse.Dat -> [String]
modres_residue_set =
  Data.List.nub
  . Data.List.sort
  . concatMap ((\(i, j) -> [i, j]) . Pdb.modres_names)
  . Pdb.Parse.dat_modres

-- | Residue sets (Atom,Hetatm,Seqres,Modres).
residue_sets :: Pdb.Parse.Dat -> ([String], [String], [String], [String])
residue_sets x = (atom_residue_set x, hetatm_residue_set x, seqres_residue_set x, modres_residue_set x)

-- | Set of 'residue_sets'
residue_sets_concat :: Pdb.Parse.Dat -> [String]
residue_sets_concat =
  Data.List.nub
  . Data.List.sort
  . concat
  . (\(a, b, c, d) -> [a, b, c, d])
  . residue_sets

-- | Atom/residue stat for selector predicate.
atom_residue_stat_of :: (Pdb.Atom -> Bool) -> FilePath -> IO ()
atom_residue_stat_of predicate pdb_fn = do
  d <- Pdb.Parse.pdb_load_dat pdb_fn
  let a = filter predicate (filter Pdb.atom_sel_altloc_A (Pdb.Parse.dat_atom_all d))
  print ("N-ATOM", length a)
  print ("N-RESIDUES", length (Data.List.nub (Data.List.sort (map Pdb.atom_residue_id a))))

-- * Water

is_water :: Pdb.Atom -> Bool
is_water = (==) "HOH" . Pdb.residue_id_name . Pdb.atom_residue_id
