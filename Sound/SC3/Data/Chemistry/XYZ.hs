-- | XYZ file format.
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

xyz_degree :: XYZ -> Int
xyz_degree (k,_,_) = k

-- | k == |e|
xyz_is_valid :: XYZ -> Bool
xyz_is_valid (k,_dsc,ent) = k == length ent

-- | Set of atoms present.
xyz_atoms :: XYZ -> [String]
xyz_atoms (_,_,ent) = nub (sort (map fst ent))

xyz_parse_entry :: String -> String -> (String,(R,R,R))
xyz_parse_entry fn s =
  case words s of
    [a,x,y,z] -> (a,(read_r x,read_r y,read_r z))
    _ -> error ("xyz_parse_entry: " ++ fn)

xyz_load :: FilePath -> IO XYZ
xyz_load fn = do
  s <- readFile fn
  case lines s of
    k:dsc:ent -> return (read k,dsc,map (xyz_parse_entry fn) ent)
    _ -> error ("xyz_load: " ++ fn)

xyz_load_dir :: FilePath -> IO [(String, XYZ)]
xyz_load_dir dir = do
  l <- listDirectory dir
  let fn = filter ((==) ".xyz" . takeExtension) l
      nm = map takeBaseName fn
  dat <- mapM (xyz_load . (</>) dir) fn
  return (zip nm dat)

xyz_map :: (V3 R -> V3 R) -> XYZ -> XYZ
xyz_map f (k,dsc,ent) = (k,dsc,map (\(a,pt) -> (a,f pt)) ent)

xyz_rotate_y :: R -> XYZ -> XYZ
xyz_rotate_y ph = xyz_map (v3_rotate_y ph)
