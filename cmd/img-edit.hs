import System.Environment {- base -}
import System.FilePath {- filepath -}

import qualified Sound.SC3.Data.Bitmap.PBM as P {- hsc3-data -}
import qualified Sound.SC3.Data.Bitmap.Type as B {- hsc3-data -}
import qualified Sound.SC3.Data.Image.Plain as I {- hsc3-data -}

-- > img_bw_le "/home/rohan/uc/sp-id/eof/png/gs/02.png"
img_bw_le :: FilePath -> IO ()
img_bw_le fn = do
  i <- I.img_load fn
  print ("(w/nc,h/nr)",I.img_dimensions i)
  let b = B.bitindices_leading_edges . B.bitarray_to_bitindices . I.img_bw_to_bitarray $ i
  P.pbm4_write (fn <.> "le.pbm") (P.bitindices_to_pbm b)

help :: String
help =
    unlines
    ["img-edit bw/le file-name"
    ," bw/le = black & white, leading edges transform"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["bw/le",fn] -> img_bw_le fn
    _ -> putStrLn help
