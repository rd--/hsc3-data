{- | Mp, <https://materialsproject.org/>

A. Jain et. al.
The Materials Project: A materials genome approach to accelerating materials innovation
APL Materials, 2013, 1(1), 011002.
<http://dx.doi.org/10.1063/1.4812323>
-}
module Sound.Sc3.Data.Chemistry.Mp where

import System.FilePath {- filepath -}

import qualified Sound.Sc3.Data.Chemistry.Poscar as Poscar {- hsc3-data -}
import qualified Sound.Sc3.Data.Chemistry.Struct as Struct {- hsc3-data -}

-- | Local Mp directory
mp_dir :: FilePath
mp_dir = "/home/rohan/sw/hsc3-data/data/chemistry/mp"

-- | Local Mp Poscar file
mp_poscar_file :: String -> FilePath
mp_poscar_file nm = mp_dir </> "poscar" </> nm <.> "poscar"

{- | Load an Mp Poscar file as a Struct.

>>> s <- mp_load "mp-541848_B"
>>> Struct.struct_degree s
(324,0)
-}
mp_load :: String -> IO Struct.Struct
mp_load nm = do
  p <- Poscar.poscar_load (mp_poscar_file nm)
  return (Struct.poscar_to_struct Poscar.Poscar_C (nm, p))

-- | Separate at first occurence of /e/ in /l/.
sep1 :: Eq t => t -> [t] -> ([t], [t])
sep1 e x =
  case break (== e) x of
    (i, _ : j) -> (i, j)
    _ -> error "sep1"

{- | Mp names have a prefix, an ID and a descriptor.

>>> mp_name_split "mp-571298_SiC"
("571298","SiC")
-}
mp_name_split :: String -> (String, String)
mp_name_split nm =
  case nm of
    'm' : 'p' : '-' : x -> sep1 '_' x
    _ -> error "mp_name_split"
