import System.Environment {- base -}
import Sound.SC3.Data.Image.Plain {- hsc3-data -}

img_to_pgm :: Int -> (RGB24 -> GREY) -> FilePath -> FilePath -> IO ()
img_to_pgm d to_grey img_fn pgm_fn = do
  i <- img_load img_fn
  img_write_pgm5 d to_grey pgm_fn i

help :: String
help = "img-to-pgm 8|16 {eq | lm/rec.709} img-file pbm-file"

main :: IO ()
main = do
  a <- getArgs
  case a of
    [d,"eq",img_fn,pgm_fn] -> img_to_pgm (read d) rgb8_to_gs_eq' img_fn pgm_fn
    [d,"lm/rec.709",img_fn,pgm_fn] -> img_to_pgm (read d) rgb_to_gs_rec_709 img_fn pgm_fn
    _ -> putStrLn help
