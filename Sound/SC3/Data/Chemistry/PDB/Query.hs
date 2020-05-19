module Sound.SC3.Data.Chemistry.PDB.Query where

import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import Data.CG.Minus.Plain {- hcg-minus -}

import Sound.SC3.Data.Chemistry.PDB.Parse {- hsc3-data -}
import Sound.SC3.Data.Chemistry.PDB.Types {- hsc3-data -}

-- * STAT

pdb_stat :: PDB -> [(String,String)]
pdb_stat ((h1,h2,h3),t,m,_,(a,h),c,sq,hlx,sht,lnk,ssb) =
  let e = nub (sort (map atom_element (a ++ h)))
      uniq_ch = map fst . nubBy ((==) `on` snd)
      u = uniq_ch sq
      alt = map atom_altloc (a ++ h)
      res = nub (sort (map atom_residue_id (a ++ h)))
      hoh = filter (\(nm,_,_,_) -> nm == "HOH") res
  in [("ID",h3)
     ,("CLASSIFICATION",h1)
     ,("DEPOSITION-DATE",h2)
     ,("TITLE",t)
     ,("NUMMDL",show (fromMaybe 1 m))] ++
     if m == Nothing
     then [("N-ATOM",show (length a))
          ,("N-HETATM",show (length h))
          ,("ATOM-ALT-ID",altloc_id_set alt)
          ,("N-ATOM-ALT",show (length (filter ((/=) ' ') alt)))
          ,("N-CHAIN",show (length sq))
          ,("N-UNIQ-CHAIN",show (length u))
          ,("CHAIN-ID-SEQ",map fst sq)
          ,("UNIQ-CHAIN-ID-SEQ",u)
          ,("N-ELEMENTS",show (length e))
          ,("ELEMENTS",unwords e)
          ,("N-CONECT",show (length c))
          ,("N-LINK",show (length lnk))
          ,("N-SSBOND",show (length ssb))
          ,("N-HELIX",show (length hlx))
          ,("N-SHEET",show (length sht))
          ,("SEQRES-N",show (sum (map (length . snd) sq)))
          ,("RES-N",show (length res))
          ,("HOH-N",show (length hoh))]
     else []

dat_stat :: DAT -> [(String,String)]
dat_stat = pdb_stat . dat_parse

-- * ALPHA CARBON

{- | Generate Cα chains of single model PDB.
     Atoms where ALTLOC is not ' ' or 'A' are deleted.
     Atoms that are located past a TER record are deleted.
     Nucleotide chains are NOT given as NULL entries.
-}
dat_to_alpha_carbon_chains :: Bool -> DAT -> Maybe [(Char,[V3 Double])]
dat_to_alpha_carbon_chains uniq dat =
  if isJust (dat_nummdl dat)
  then Nothing
  else let t = dat_ter dat
           a = map (atom_apply_ter t) (atom_group (filter atom_sel_altloc_A (dat_atom_all dat)))
           uniq_ch = map fst . nubBy ((==) `on` snd)
           u = uniq_ch (seqres_group (dat_seqres dat))
           c = if uniq then filter (flip elem u . fst) a else a
           p = map (map atom_coord . filter ((==) "CA" . atom_name) . snd) c
       in Just (filter (not . null . snd) (zip (map fst c) p))
