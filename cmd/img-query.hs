import System.Environment {- base -}

import qualified Sound.SC3.Data.Image.Plain as Plain {- hsc3-data -}

img_uniq :: Char -> FilePath -> IO ()
img_uniq md fn = do
  i <- Plain.img_load fn
  let f (ix,c) = (ix,Plain.rgb24_unpack c)
  case md of
    'c' -> print (map (Plain.rgb24_unpack . snd) (Plain.img_uniq_colours i))
    'l' -> print (map f (Plain.img_uniq_colours i))
    'g' -> print (map (map f) (Plain.img_uniq_colours_gr i))
    _ -> error "img_uniq"

help :: String
help =
    unlines
    ["img-query uniq c|l|g img-file"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["uniq",[md],fn] -> img_uniq md fn
    _ -> putStrLn help

{-
mapM_ (\n -> img_uniq 'c' ("/home/rohan/rd/j/2017-11-30/G." ++ show n ++ ".png")) [0 .. 7]
-}
