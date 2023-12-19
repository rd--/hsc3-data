{- | Aflow, <http://aflow.org/CrystalDatabase/>

Computational Materials Science
Volume 136, Supplement, August 2017, Pages S1-S828
The Aflow Library of Crystallographic Prototypes: Part 1
Michael J.Mehl et. al.
<https://doi.org/10.1016/j.commatsci.2017.01.017>

Pt.1: <https://arxiv.org/abs/1607.02532>,
Pt.2: <https://arxiv.org/abs/1806.07864>
-}
module Sound.Sc3.Data.Chemistry.Aflow where

import qualified Music.Theory.Array.Csv as T {- hmt -}

-- | Local Aflow directory.
aflow_dir :: FilePath
aflow_dir = "/home/rohan/sw/hsc3-data/data/chemistry/aflow/"

-- | Local Aflow file.
aflow_file :: FilePath -> FilePath
aflow_file = (++) aflow_dir

-- | Aflow url for crystal given code.
aflow_uri :: String -> String
aflow_uri nm = "http://www.aflowlib.org/CrystalDatabase/" ++ nm ++ ".html"

-- | Read (index,name,code) triples from Csv file.
read_aflow_index_csv :: FilePath -> IO [(Int, String, String)]
read_aflow_index_csv fn = do
  tbl <- T.csv_table_read_def id fn
  let f r =
        case r of
          [i, j, k] -> (read i, j, k)
          _ -> error "read_aflow_index_csv"
  return (map f tbl)

-- | Read A index (ie. PART 1)
aflow_index_A :: IO [(Int, String, String)]
aflow_index_A = read_aflow_index_csv (aflow_file "csv/index.A.csv")

-- | Read A index (ie. PART 2)
aflow_index_B :: IO [(Int, String, String)]
aflow_index_B = read_aflow_index_csv (aflow_file "csv/index.B.csv")
