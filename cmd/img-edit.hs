import System.Environment {- base -}

import qualified Sound.SC3.Data.Bitmap.PBM as P {- hsc3-data -}
import qualified Sound.SC3.Data.Bitmap.Type as B {- hsc3-data -}
import qualified Sound.SC3.Data.Image.Plain as I {- hsc3-data -}

-- > let fn = "/home/rohan/uc/sp-id/eof/png/gs/03.png"
-- > let dir = B.DOWN
-- > img_bw_le dir fn (fn ++ "." ++ B.direction_pp dir ++ ".pbm")
img_bw_le :: B.DIRECTION -> FilePath -> FilePath -> IO ()
img_bw_le dir img_fn pbm_fn = do
  i <- I.img_load img_fn
  print ("(w/nc,h/nr)",I.img_dimensions i)
  let b = B.bitindices_leading_edges dir . B.bitarray_to_bitindices . I.img_bw_to_bitarray $ i
  P.pbm4_write pbm_fn (P.bitindices_to_pbm b)

help :: String
help =
    unlines
    ["img-edit bw/le {r|l|d|u} img-file pbm-file"
    ," bw/le = black & white, leading edges transform"
    ," r = right, l = left, d = down, u = up"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["bw/le",[dir],img_fn,pbm_fn] -> img_bw_le (B.parse_dir_char' dir) img_fn pbm_fn
    _ -> putStrLn help
