{- | Poscar file format.

<https://cms.mpi.univie.ac.at/vasp/guide/node59.html>.
Coordinates are in Angstroms.
Connection data is not present.
-}
module Sound.Sc3.Data.Chemistry.Poscar where

import Data.Bifunctor {- base -}
import Data.Char {- base -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}

import Music.Theory.Geometry.Vector {- hmt-base -}

-- | Parse three-element real-valued vector.
poscar_parse_r3 :: String -> V3 Double
poscar_parse_r3 s =
  case words s of
    [i,j,k] -> (read i,read j,read k)
    _ -> error "poscar_parse_r3"

-- | Parse co-ordinate and atomic-symbol, ignore remainder of line.
poscar_parse_atom :: String -> (V3 Double,String)
poscar_parse_atom s =
  case words s of
    i:j:k:sym:_ -> ((read i,read j,read k),sym)
    _ -> error "poscar_parse_atom"

-- | 3×3 lattice.
type Lattice = V3 (V3 Double)

-- | Convert direct coordinate to cartesian.
poscar_direct_to_cartesian :: Lattice -> V3 Double -> V3 Double
poscar_direct_to_cartesian (a1,a2,a3) (i,j,k) =
  v3_scale i a1 `v3_add` v3_scale j a2 `v3_add` v3_scale k a3

-- | Direct or cartesian co-ordinates.
data Poscar_Ty = Poscar_D | Poscar_C deriving (Eq,Enum,Show)

-- | (Description,U,Lattice,Atom-Histogram,Atom-Data)
type Poscar = (String,Double,Lattice,[(String,Int)],Poscar_Ty,[(V3 Double,String)])

poscar_description :: Poscar -> String
poscar_description (dsc,_,_,_,_,_) = dsc

poscar_degree :: Poscar -> Int
poscar_degree (_,_,_,h,_,_) = sum (map snd h)

poscar_atom_data :: Poscar -> [(V3 Double,String)]
poscar_atom_data (_,_,_,_,_,a) = a

poscar_atoms_cartesian :: Poscar -> [(V3 Double,String)]
poscar_atoms_cartesian (_,_,l,_,ty,a) =
  case ty of
    Poscar_D -> map (first (poscar_direct_to_cartesian l)) a
    Poscar_C -> error "poscar_atoms_cartestian"

poscar_atoms_direct :: Poscar -> [(V3 Double,String)]
poscar_atoms_direct (_,_,_,_,ty,a) =
  case ty of
    Poscar_D -> a
    Poscar_C -> error "poscar_atoms_direct"

poscar_atoms :: Poscar_Ty -> Poscar -> [(V3 Double,String)]
poscar_atoms ty =
  case ty of
    Poscar_D -> poscar_atoms_direct
    Poscar_C -> poscar_atoms_cartesian

poscar_parse :: String -> Poscar
poscar_parse s =
  case lines s of
    dsc:u:l0:l1:l2:a_nm:a_cnt:ty:dat ->
      let a_cnt' = map read (words a_cnt)
          ty' = if map toLower ty == "direct" then Poscar_D else error "poscar_parse"
      in (dsc
         ,read u
         ,(poscar_parse_r3 l0,poscar_parse_r3 l1,poscar_parse_r3 l2)
         ,zip (words a_nm) a_cnt'
         ,ty'
         ,map poscar_parse_atom (take (sum a_cnt') dat))
    _ -> error "poscar_parse"

-- | (minima,maxima) of atoms.
poscar_bounds :: Poscar -> V2 (V3 Double)
poscar_bounds p =
  let c = map fst (poscar_atoms_cartesian p)
      r = unzip3 c
  in (v3_map minimum r,v3_map maximum r)

{- | Load ".poscar" file.

>>> p <- poscar_load "/home/rohan/sw/hsc3-data/data/chemistry/aflow/poscar/A3B_cI32_204_g_c.poscar"
>>> poscar_bounds p
((-1.895,-1.895,-1.895),(4.979302000000001,4.979302000000001,4.979302000000001))
-}
poscar_load :: FilePath -> IO Poscar
poscar_load = fmap poscar_parse . readFile

-- | List of all ".poscar" files at /dir/.
poscar_dir_entries :: FilePath -> IO [FilePath]
poscar_dir_entries = fmap (filter ((==) ".poscar" . takeExtension)) . listDirectory

-- | Load all ".poscar" files at directory.
poscar_load_dir :: FilePath -> IO [(String, Poscar)]
poscar_load_dir dir = do
  fn <- poscar_dir_entries dir
  let nm = map takeBaseName fn
  dat <- mapM (poscar_load . (</>) dir) fn
  return (zip nm dat)
