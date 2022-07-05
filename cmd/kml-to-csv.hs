import Data.List {- base -}
import System.Environment {- base -}
import Text.Printf {- base -}

import Sound.Sc3.Data.XML.KML {- hsc3-data -}

minmax :: Ord x => [x] -> (x,x)
minmax x = (minimum x,maximum x)

-- > kml_stat "/home/rohan/bnt.kml"
kml_stat :: FilePath -> IO ()
kml_stat kml_fn = do
  c <- kml_load_coordinates kml_fn
  let c' = concat c
  print ("# coordinate sets",length c)
  print ("# coordinates",length c')
  print ("# coordinates/set",map length c)
  let (x,y,z) = unzip3 c'
  print ("min|max",minmax x,minmax y,minmax z)
  let (x',y',z') = unzip3 (map unzip3 c)
  print ("min|max/set",map minmax x',map minmax y',map minmax z')

-- > kml_to_csv_concat "/home/rohan/bnt.kml" "/home/rohan/bnt.csv"
kml_to_csv_concat :: FilePath -> FilePath -> IO ()
kml_to_csv_concat kml_fn csv_fn = do
  c <- kml_load_coordinates kml_fn
  let f (p,q,r) = intercalate "," (map show [p,q,r])
      s' = map f (concat c)
  writeFile csv_fn (unlines s')

-- > kml_to_csv_split "/home/rohan/bnt.kml" "/home/rohan/bnt"
kml_to_csv_split :: FilePath -> FilePath -> IO ()
kml_to_csv_split kml_fn csv_fn = do
  c <- kml_load_coordinates kml_fn
  let f (p,q,r) = intercalate "," (map show [p,q,r])
      gen_nm :: Int -> String
      gen_nm n = printf "%s.%03d.csv" csv_fn n
      g (c',n) = writeFile (gen_nm n) (unlines (map f c'))
  mapM_ g (zip c [0..])

help :: [String]
help =
    ["kml-to-csv"
    ,"  concat kml-file csv-file"
    ,"  split kml-file csv-file-prefix"
    ,"  stat kml-file"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["stat",kml_fn] -> kml_stat kml_fn
    ["concat",kml_fn,csv_fn] -> kml_to_csv_concat kml_fn csv_fn
    ["split",kml_fn,csv_fn] -> kml_to_csv_split kml_fn csv_fn
    _ -> putStrLn (unlines help)
