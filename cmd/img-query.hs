import System.Environment {- base -}

import qualified Sound.SC3.Data.Image.Plain as Plain {- hsc3-data -}

img_uniq :: FilePath -> IO ()
img_uniq fn = do
  i <- Plain.img_load fn
  let u = Plain.img_uniq_colours_gr i
      f (ix,c) = (ix,Plain.rgb24_unpack c)
  print (map length u)
  print (map (map f) u)

help :: String
help =
    unlines
    ["img-query uniq img-file"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["uniq",fn] -> img_uniq fn
    _ -> putStrLn help

{-
img_uniq "/home/rohan/I-008.png"
-}
