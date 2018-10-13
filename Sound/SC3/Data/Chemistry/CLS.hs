{- | Crystal Lattice Structures, <http://homepage.univie.ac.at/michael.leitner/lattice/>

Crystal Lattice Structures
U.S. Naval Research Laboratory
Center for Computational Materials Science

-}
module Sound.SC3.Data.Chemistry.CLS where

cls_dir :: FilePath
cls_dir = "/home/rohan/sw/hsc3-data/data/chemistry/cls/"

cls_file :: FilePath -> FilePath
cls_file = (++) cls_dir
