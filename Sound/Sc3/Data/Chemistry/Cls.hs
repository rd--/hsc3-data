{- | Crystal Lattice Structures, <http://homepage.univie.ac.at/michael.leitner/lattice/>

Crystal Lattice Structures
U.S. Naval Research Laboratory
Center for Computational Materials Science
-}
module Sound.Sc3.Data.Chemistry.Cls where

import Data.Maybe {- base -}
import System.Environment {- base -}
import System.FilePath {- filepath -}

import Sound.Sc3.Data.Chemistry.Struct {- hsc3-data -}
import qualified Sound.Sc3.Data.Chemistry.Xyz as Xyz {- hsc3-data -}

-- | Lookup Cls_DIR, or default to local directory.
cls_dir :: IO FilePath
cls_dir = do
  r <- lookupEnv "CLS_DIR"
  return (fromMaybe "/home/rohan/sw/hsc3-data/data/chemistry/cls/" r)

-- | Make Cls xyz filename from ID.
cls_xyz_file :: String -> FilePath
cls_xyz_file nm = "xyz" </> nm <.> "xyz"

{- | Load a Cls Xyz file as a Struct.

>>> s <- cls_load "Bergman"
>>> struct_bounds s
((-7.08,-7.08,-7.08),(7.08,7.08,7.08))
-}
cls_load :: String -> IO Struct
cls_load nm = do
  d <- cls_dir
  x <- Xyz.xyz_load (d </> cls_xyz_file nm)
  return (xyz_to_struct (nm, x))

{- | Load all Cls Xyz files as Structs.

>>> sq <- cls_load_dir
>>> length sq
268
-}
cls_load_dir :: IO [Struct]
cls_load_dir = do
  d <- cls_dir
  s <- Xyz.xyz_load_dir (d </> "xyz")
  return (map xyz_to_struct s)
