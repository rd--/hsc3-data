-- | XYZ file format.  Coordinates are ordinarily Angstroms.
module Sound.SC3.Data.Chemistry.XYZ where

import Data.List {- base -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}

import Data.CG.Minus.Plain {- hcg-minus -}

-- | Allow elided 0 before decimal place.
--
-- > map read_r ["-.5",".5"]
read_r :: String -> R
read_r s =
  case s of
    '-':'.':s' -> read ('-':'0':'.':s')
    '.':_ -> read ('0':s)
    _ -> read s

-- | (atomic-symbol,xyz-coordinate)
type ATOM = (String,V3 R)

-- | (k = n-atoms,d = description,e = [atom]
type XYZ = (Int,String,[ATOM])

-- | Numbner of atoms.
xyz_degree :: XYZ -> Int
xyz_degree (k,_,_) = k

-- | k == |atoms|
xyz_is_valid :: XYZ -> Bool
xyz_is_valid (k,_dsc,ent) = k == length ent

-- | Set of atoms present.
xyz_atom_set :: XYZ -> [String]
xyz_atom_set (_,_,ent) = nub (sort (map fst ent))

{- | The first line is the number of atoms.  This may be preceded by
whitespace and anything following is ignored. -}
xyz_parse_cnt :: String -> Int
xyz_parse_cnt s =
  case words s of
    k:_ -> read k
    _ -> error ("xyz_parse_cnt: " ++ s)

{- | Each entry describing an atom must contain at least four fields of
information separated by whitespace: the atom's type and its X, Y, and
Z positions. -}
xyz_parse_entry :: String -> String -> (String,(R,R,R))
xyz_parse_entry fn s =
  case words s of
    a:x:y:z:_ -> (a,(read_r x,read_r y,read_r z))
    _ -> error ("xyz_parse_entry: " ++ fn)

-- | Parse ".xyz" file.
xyz_parse :: FilePath -> String -> XYZ
xyz_parse fn s =
  case lines s of
    k:dsc:ent -> (xyz_parse_cnt k,dsc,map (xyz_parse_entry fn) ent)
    _ -> error ("xyz_parse: " ++ fn)

-- | (minima,maxima) of atoms.
xyz_bounds :: XYZ -> V2 (V3 R)
xyz_bounds (_,_,a) =
  let c = map snd a
      r = unzip3 c
  in (v3_uop minimum r,v3_uop maximum r)

-- | Load ".xyz" file.
--
-- > xyz <- xyz_load "/home/rohan/data/chemistry/cls/xyz/Al12W.xyz"
-- > xyz_bounds xyz == ((0.0,7.5803),(0.0,7.5803),(0.0,7.5803))
xyz_load :: FilePath -> IO XYZ
xyz_load fn = fmap (xyz_parse fn) (readFile fn)

-- | List of all ".xyz" files at /dir/.
xyz_dir_entries :: FilePath -> IO [FilePath]
xyz_dir_entries = fmap (filter ((==) ".xyz" . takeExtension)) . listDirectory

-- | Load all ".xyz" files at /dir/.
xyz_load_dir :: FilePath -> IO [(String, XYZ)]
xyz_load_dir dir = do
  fn <- xyz_dir_entries dir
  let nm = map takeBaseName fn
  dat <- mapM (xyz_load . (</>) dir) fn
  return (zip nm dat)
