-- | POSCAR file format.  Coordinates are in Angstroms.
module Sound.SC3.Data.Chemistry.POSCAR where

import Data.Char {- base -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}

import Data.CG.Minus.Plain {- hcg-minus -}

-- | Parse three-element vector.
poscar_parse_r3 :: String -> (V3 R)
poscar_parse_r3 s =
  case words s of
    [i,j,k] -> (read i,read j,read k)
    _ -> error "poscar_parse_r3"

-- | Parse co-ordinate and atomic-symbol.
poscar_parse_atom :: String -> (V3 R,String)
poscar_parse_atom s =
  case words s of
    i:j:k:sym:_ -> ((read i,read j,read k),sym)
    _ -> error "poscar_parse_atom"

-- | 3×3 lattice.
type LATTICE = V3 (V3 R)

-- | Convert DIRECT coordinate to cartesian.
poscar_direct_to_cartesian :: LATTICE -> V3 R -> V3 R
poscar_direct_to_cartesian (a1,a2,a3) (i,j,k) =
  (v3_scale i a1) `v3_add` (v3_scale j a2) `v3_add` (v3_scale k a3)

-- | Direct or caretsian co-ordinates.
data POSCAR_TY = POSCAR_D | POSCAR_C

-- | (description,u,lattice,atom-histogram,atom-data)
type POSCAR = (String,R,LATTICE,[(String,Int)],POSCAR_TY,[(V3 R,String)])

poscar_description :: POSCAR -> String
poscar_description (dsc,_,_,_,_,_) = dsc

poscar_degree :: POSCAR -> Int
poscar_degree (_,_,_,h,_,_) = sum (map snd h)

poscar_atom_data :: POSCAR -> [(V3 R,String)]
poscar_atom_data (_,_,_,_,_,a) = a

poscar_atoms_cartesian :: POSCAR -> [(V3 R,String)]
poscar_atoms_cartesian (_,_,l,_,ty,a) =
  case ty of
    POSCAR_D -> map (\(v,sym) -> (poscar_direct_to_cartesian l v,sym)) a
    POSCAR_C -> error "poscar_atoms_cartestian"

poscar_atoms_direct :: POSCAR -> [(V3 R,String)]
poscar_atoms_direct (_,_,_,_,ty,a) =
  case ty of
    POSCAR_D -> a
    POSCAR_C -> error "poscar_atoms_direct"

poscar_atoms :: POSCAR_TY -> POSCAR -> [(V3 R,String)]
poscar_atoms ty =
  case ty of
    POSCAR_D -> poscar_atoms_direct
    POSCAR_C -> poscar_atoms_cartesian

poscar_parse :: String -> POSCAR
poscar_parse s =
  case lines s of
    dsc:u:l0:l1:l2:a_nm:a_cnt:ty:dat ->
      let a_cnt' = map read (words a_cnt)
          ty' = if map toLower ty == "direct" then POSCAR_D else error "poscar_parse"
      in (dsc
         ,read u
         ,(poscar_parse_r3 l0,poscar_parse_r3 l1,poscar_parse_r3 l2)
         ,zip (words a_nm) a_cnt'
         ,ty'
         ,map poscar_parse_atom (take (sum a_cnt') dat))
    _ -> error "poscar_parse"

-- | (minima,maxima) of atoms.
poscar_bounds :: POSCAR -> V2 (V3 R)
poscar_bounds p =
  let c = map fst (poscar_atoms_cartesian p)
      r = unzip3 c
  in (v3_uop minimum r,v3_uop maximum r)

-- | Load ".poscar" file.
--
-- > p <- poscar_load "/home/rohan/data/chemistry/aflow/poscar/A3B_cI32_204_g_c.poscar"
-- > poscar_bounds p == let r = (-1.895,4.979302000000001) in (r,r,r)
poscar_load :: FilePath -> IO POSCAR
poscar_load = fmap poscar_parse . readFile

-- | Load all ".poscar" files at directory.
poscar_load_dir :: FilePath -> IO [(String, POSCAR)]
poscar_load_dir dir = do
  l <- listDirectory dir
  let fn = filter ((==) ".poscar" . takeExtension) l
      nm = map takeBaseName fn
  dat <- mapM (poscar_load . (</>) dir) fn
  return (zip nm dat)
