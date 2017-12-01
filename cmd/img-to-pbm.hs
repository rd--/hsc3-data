import System.Environment {- base -}
import Sound.SC3.Data.Image.Plain {- hsc3-data -}

img_to_pbm :: (RGB24 -> BW) -> FilePath -> FilePath -> IO ()
img_to_pbm to_bw img_fn pbm_fn = do
  i <- img_load img_fn
  img_bw_write_pbm4 to_bw pbm_fn i

help :: String
help =
    unlines
    ["img-to-pbm eq img-file pbm-file"
    ,"img-to-pbm lm/rec.709 th:real img-file pbm-file"]

read_double :: String -> Double
read_double = read

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["eq",img_fn,pbm_fn] -> img_to_pbm rgb8_to_bw_eq' img_fn pbm_fn
    ["lm/rec.709",th,img_fn,pbm_fn] ->
        let f = (< (read_double th)) . rgb_to_gs_rec_709
        in img_to_pbm f img_fn pbm_fn
    _ -> putStrLn help
