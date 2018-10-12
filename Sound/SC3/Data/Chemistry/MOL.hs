-- | MOL file format.
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

mol_read_atom :: String -> (V3 R,String)
mol_read_atom s =
  case words s of
    [x,y,z,a,_,_,_,_,_,_,_,_,_,_,_,_] -> ((read x,read y,read z),a)
    _ -> error "mol_read_atom"

mol_read_bond :: String -> (V2 Int,Int)
mol_read_bond s =
  case words s of
    [a0,a1,ty,_,_,_,_] -> ((read a0,read a1),read ty)
    _ -> error "mol_read_bond"

type ATOM = (V3 R, String)
type BOND = (V2 Int, Int)

-- | (name,description,atom-count,bond-count,atoms,bonds)
type MOL = (String, String, Int, Int, [ATOM], [BOND])

mol_parse :: String -> MOL
mol_parse s =
  let nm:dsc:_:cnt:dat = lines s
      (a_n,b_n) = mol_read_counts cnt
      a = map mol_read_atom (take a_n dat)
      b = map mol_read_bond (take b_n (drop a_n dat))
  in (nm,dsc,a_n,b_n,a,b)

mol_load :: FilePath -> IO MOL
mol_load fn = readFile fn >>= return . mol_parse

-- | Load all ".mol" files at directory.
mol_load_dir :: FilePath -> IO [(String, MOL)]
mol_load_dir dir = do
  l <- listDirectory dir
  let fn = filter ((==) ".mol" . takeExtension) l
      nm = map takeBaseName fn
  dat <- mapM (mol_load . (</>) dir) fn
  return (zip nm dat)
