{- | Crystal Lattice Structures, <http://homepage.univie.ac.at/michael.leitner/lattice/>

Crystal Lattice Structures
U.S. Naval Research Laboratory
Center for Computational Materials Science

-}
module Sound.SC3.Data.Chemistry.CLS where

import System.FilePath {- filepath -}

import Sound.SC3.Data.Chemistry.STRUCT {- hsc3-data -}
import qualified Sound.SC3.Data.Chemistry.XYZ as XYZ {- hsc3-data -}

cls_dir :: FilePath
cls_dir = "/home/rohan/sw/hsc3-data/data/chemistry/cls/"

cls_file :: FilePath -> FilePath
cls_file = (++) cls_dir

-- | Make CLS xyz filename from ID.
cls_xyz_file :: String -> FilePath
cls_xyz_file nm = cls_file ("xyz" </> nm <.> "xyz")

-- | Load a CLS XYZ file as a STRUCT.
--
-- > s <- cls_load "Bergman"
-- > struct_bounds s
cls_load :: String -> IO STRUCT
cls_load nm = do
  x <- XYZ.xyz_load (cls_xyz_file nm)
  return (xyz_to_struct (nm,x))

-- | Load a all CLS XYZ files as STRUCTs.
--
-- > sq <- cls_load_dir
-- > length sq == 268
cls_load_dir :: IO [STRUCT]
cls_load_dir = do
  s <- XYZ.xyz_load_dir (cls_file "xyz")
  return (map xyz_to_struct s)
