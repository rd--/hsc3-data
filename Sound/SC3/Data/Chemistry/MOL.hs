{- | MOL file format.

Dalby, A. et al. (1992). "Description of several chemical structure
file formats used by computer programs developed at Molecular Design
Limited". Journal of Chemical Information and Modeling. 32 (3): 244.

<https://pubs.acs.org/doi/abs/10.1021/ci00007a012>
-}
module Sound.SC3.Data.Chemistry.MOL where

import System.Directory {- directory -}
import System.FilePath {- filepath -}

import Data.CG.Minus.Plain {- hcg-minus -}

mol_read_counts :: String -> (Int,Int)
mol_read_counts s =
  case words s of
    [a,b,"0",_d,"0","0","0","0","0999","V2000"] -> (read a,read b)
    [a,b,"0","0","0","0","0","0","0","0999","V2000"] -> (read a,read b)
    _ -> error ("mol_read_counts: " ++ s)

-- | (xyz-coordinate, atomic-symbol)
type MOL_ATOM = (V3 Double, String)

mol_read_atom :: String -> MOL_ATOM
mol_read_atom s =
  case words s of
    [x,y,z,a,_,_,_,_,_,_,_,_,_,_,_,_] -> ((read x,read y,read z),a)
    _ -> error "mol_read_atom"

-- | MOL files include BOND data.
--   MOL bond data is ONE-INDEXED.
type MOL_BOND = (V2 Int, Int, Int)

mol_bond_type_tbl :: [(Int,String)]
mol_bond_type_tbl = [(1,"Single"),(2,"Double"),(3,"Triple"),(4,"Aromatic")]

mol_bond_stereo_tbl :: [(Int,String)]
mol_bond_stereo_tbl = [(0,"Not stereo"),(1,"Up"),(6,"Down")]

mol_read_bond :: String -> MOL_BOND
mol_read_bond s =
  case words s of
    [a0,a1,ty,sc,_,_,_] -> ((read a0,read a1),read ty,read sc)
    _ -> error "mol_read_bond"

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

-- | List of all ".mol" files at /dir/.
mol_dir_entries :: FilePath -> IO [FilePath]
mol_dir_entries = fmap (filter ((==) ".mol" . takeExtension)) . listDirectory

-- | Load all ".mol" files at directory.
mol_load_dir :: FilePath -> IO [(String, MOL)]
mol_load_dir dir = do
  fn <- mol_dir_entries dir
  let nm = map takeBaseName fn
  dat <- mapM (mol_load . (</>) dir) fn
  return (zip nm dat)
