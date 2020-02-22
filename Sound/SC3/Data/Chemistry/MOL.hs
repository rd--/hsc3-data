{- | MOL/SDF file format.

Dalby, A. et al. (1992). "Description of several chemical structure
file formats used by computer programs developed at Molecular Design
Limited". Journal of Chemical Information and Modeling. 32 (3): 244.

<https://pubs.acs.org/doi/abs/10.1021/ci00007a012>

MOL fields are fixed length.
-}
module Sound.SC3.Data.Chemistry.MOL where

import Data.Char {- base -}
import Data.List.Split {- split -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}

import Data.CG.Minus.Plain {- hcg-minus -}

-- > length mol_counts_flen == 12
-- > sum mol_counts_flen == 39
mol_counts_flen :: [Int]
mol_counts_flen = replicate 11 3 ++ [6]

mol_read_counts :: String -> (Int,Int)
mol_read_counts s =
  case splitPlaces mol_counts_flen s of
    [a,b,"  0","   ","  0","  0","  0","  0","  0","  0","999"," V2000"] -> (read a,read b)
    [a,b,    _,    _,    _,    _,    _,    _,    _,    _,    _," V2000"] -> (read a,read b)
    r -> error ("mol_read_counts: " ++ s ++ show r)

-- | (xyz-coordinate, atomic-symbol)
type MOL_ATOM = (V3 Double, String)

-- > length mol_atom_flen == 17
-- > sum mol_atom_flen == 69
mol_atom_flen :: [Int]
mol_atom_flen = [10,10,10,1,3,2,3] ++ replicate 10 3

mol_read_atom :: String -> MOL_ATOM
mol_read_atom s =
  case splitPlaces mol_atom_flen s of
    [x,y,z," ",a,_,_,_,_,_,_,_,_,_,_,_,_] -> ((read x,read y,read z),takeWhile (not . isSpace) a)
    r -> error ("mol_read_atom: " ++ s ++ show r)

-- | MOL files include BOND data.
--   MOL bond data is ONE-INDEXED.
type MOL_BOND = (V2 Int, Int, Int)

mol_bond_type_tbl :: [(Int,String)]
mol_bond_type_tbl = [(1,"Single"),(2,"Double"),(3,"Triple"),(4,"Aromatic")]

mol_bond_stereo_tbl :: [(Int,String)]
mol_bond_stereo_tbl = [(0,"Not stereo"),(1,"Up"),(6,"Down")]

-- > length mol_bond_flen == 7
-- > sum mol_bond_flen == 21
mol_bond_flen :: [Int]
mol_bond_flen = replicate 7 3

mol_read_bond :: String -> MOL_BOND
mol_read_bond s =
  case splitPlaces mol_bond_flen s of
    [a0,a1,ty,sc,_,_,_] -> ((read a0,read a1),read ty,read sc)
    _ -> error ("mol_read_bond: " ++ s)

-- | (name,description,atom-count,bond-count,atoms,bonds)
type MOL = (String, String, Int, Int, [MOL_ATOM], [MOL_BOND])

mol_parse :: String -> MOL
mol_parse s =
  let nm:dsc:_:cnt:dat = lines s
      (a_n,b_n) = mol_read_counts cnt
      a = map mol_read_atom (take a_n dat)
      b = map mol_read_bond (take b_n (drop a_n dat))
  in (nm,dsc,a_n,b_n,a,b)

mol_load :: FilePath -> IO MOL
mol_load fn = readFile fn >>= return . mol_parse

-- | List of all .ext files at /dir/.  SDF is a superset of MOL, extensions are ".mol" and ".sdf".
mol_dir_entries :: String -> FilePath -> IO [FilePath]
mol_dir_entries ext = fmap (filter ((==) ext . takeExtension)) . listDirectory

-- | Load all .ext files at directory.
mol_load_dir :: String -> FilePath -> IO [(String, MOL)]
mol_load_dir ext dir = do
  fn <- mol_dir_entries ext dir
  let nm = map takeBaseName fn
  dat <- mapM (mol_load . (</>) dir) fn
  return (zip nm dat)

{-

let fn = "/home/rohan/rd/j/2019-10-08/sdf/5288826.sdf"
m <- mol_load fn
m

-}
