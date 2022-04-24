import System.Environment {- base -}
import qualified Text.CSV.Lazy.String as C {- lazy-csv -}

import Sound.SC3.Data.Bitmap.Type {- hsc3-data -}
import Sound.SC3.Data.Bitmap.PBM {- hsc3-data -}

csv_load :: (String -> n) -> FilePath -> IO [[n]]
csv_load f fn = do
  s <- readFile fn
  let t = C.fromCSVTable (C.csvTable (C.parseCSV s))
  return (map (map f) t)

csv_load_double :: FilePath -> IO [[Double]]
csv_load_double = csv_load read

{-
csv_load_int :: FilePath -> IO [[Int]]
csv_load_int = csv_load read
-}

csv_load_double_round :: FilePath -> IO [[Int]]
csv_load_double_round = fmap (map (map round)) . csv_load_double

csv_to_indices :: (Int,Int) -> [[t]] -> [(t, t)]
csv_to_indices (i,j) =
    let f r = (r !! i,r !! j)
    in map f

mk_pt_real_pbm :: FilePath -> Dimensions -> (Int,Int) -> FilePath -> IO ()
mk_pt_real_pbm csv_fn dm ix pbm_fn = do
  dat <- csv_load_double_round csv_fn
  let ind = csv_to_indices ix dat
  write_pbm_bitindices pbm_fn (dm,ind)

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["pt","real","pbm",csv_fn,h,w,i,j,pbm_fn] -> mk_pt_real_pbm csv_fn (read h,read w) (read i,read j) pbm_fn
    _ -> putStrLn "csv-to-img {pt} {real} {pbm} csv-file height width y-index x-index pbm-file"

{-

let csv_fn = "/home/rohan/cvs/uc/uc-26/daily-practice/2016-07-06/pt-2-20.csv"
mk_pt_real_pbm csv_fn (1001,1001) (0,1) "/tmp/pt-2-20.pbm"

-}
