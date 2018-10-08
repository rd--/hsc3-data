-- | MOL file format.
module Sound.SC3.Data.Chemistry.MOL where

import Data.CG.Minus.Plain {- hcg-minus -}

mol_read_counts :: String -> (Int,Int)
mol_read_counts s =
  case words s of
    [a,b,_,_,_,_,_,_,_,_,"V2000"] -> (read a,read b)
    _ -> error "mol_read_counts"

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

type MOL = (String, String, Int, Int, [(V3 R, String)], [(V2 Int, Int)])

mol_parse :: String -> MOL
mol_parse s =
  let nm:dsc:_:cnt:dat = lines s
      (a_n,b_n) = mol_read_counts cnt
      a = map mol_read_atom (take a_n dat)
      b = map mol_read_bond (take b_n (drop a_n dat))
  in (nm,dsc,a_n,b_n,a,b)

mol_load :: FilePath -> IO MOL
mol_load fn = readFile fn >>= return . mol_parse
