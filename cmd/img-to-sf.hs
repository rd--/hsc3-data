import System.Environment {- base -}

import Sound.SC3.Data.Image.Plain

img_to_sf :: (RGB8 -> Double) -> FilePath -> IO ()
img_to_sf to_gs fn = do
  i <- img_load fn
  img_gs_write_au to_gs (fn ++ ".au") i

help :: IO ()
help =
    let txt = ["img-to-sf mode file-name"
              ," mode = bw/eq | gs/eq | gs/ch/{r|g|b} | gs/lm/rec.709"]
    in putStrLn (unlines txt)

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["bw/eq",fn] -> img_to_sf (fromIntegral . fromEnum . rgb8_to_bw_eq') fn
    ["gs/eq",fn] -> img_to_sf rgb8_to_gs_eq' fn
    ["gs/ch/r",fn] -> img_to_sf (rgb8_to_gs_ch RED) fn
    ["gs/ch/g",fn] -> img_to_sf (rgb8_to_gs_ch GREEN) fn
    ["gs/ch/b",fn] -> img_to_sf (rgb8_to_gs_ch BLUE) fn
    ["gs/lm/rec.709",fn] -> img_to_sf rgb_to_gs_rec_709 fn
    _ -> help
