import Data.List {- base -}
import System.Environment {- base -}

import qualified Sound.SC3.Data.Bitmap.PBM as I {- hsc3-data -}
import qualified Sound.SC3.Data.Bitmap.Type as B {- hsc3-data -}

pbm_load_indices :: FilePath -> IO B.Indices
pbm_load_indices pbm_fn = do
  pbm <- I.read_pbm pbm_fn
  let (_,ix) = I.pbm_to_bitindices pbm
  return ix

pbm_indices_csv :: FilePath -> FilePath -> IO ()
pbm_indices_csv pbm_fn csv_fn = do
  ix <- pbm_load_indices pbm_fn
  let f (r,c) = show r ++ "," ++ show c
  writeFile csv_fn (unlines ("r,c" : map f ix))

pbm_indices_json :: FilePath -> FilePath -> IO ()
pbm_indices_json pbm_fn json_fn = do
  ix <- pbm_load_indices pbm_fn
  let to_array l = "[" ++ intercalate "," l ++ "]"
      f (r,c) = to_array [show r,show c]
  writeFile json_fn (to_array (map f ix))

help :: String
help = unlines ["pbm-indices csv pbm-file csv-file","pbm-indices json pbm-file json-file"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["csv",pbm_fn,csv_fn] -> pbm_indices_csv pbm_fn csv_fn
    ["json",pbm_fn,json_fn] -> pbm_indices_json pbm_fn json_fn
    _ -> putStrLn help

{-
pbm_indices_csv "/home/rohan/sw/hsc3-data/data/pbm/fh.pbm" "/dev/stdout"
pbm_indices_json "/home/rohan/sw/hsc3-data/data/pbm/fh.pbm" "/dev/stdout"
-}
