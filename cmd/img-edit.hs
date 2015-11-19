import System.Environment {- base -}
import System.FilePath {- filepath -}

import qualified Sound.SC3.Data.Bitmap.PBM as P {- hsc3-data -}
import qualified Sound.SC3.Data.Bitmap.Type as B {- hsc3-data -}

edit_pbm_le :: B.DIRECTION -> FilePath -> FilePath -> IO ()
edit_pbm_le dir in_fn out_fn = do
  i <- P.read_pbm in_fn
  print ("(w/nc,h/nr)",P.pbm_dimensions i)
  let b = B.bitmap_leading_edges dir (P.pbm_to_bitmap i)
  P.pbm4_write out_fn (P.bitmap_to_pbm b)

-- > let fn = "/home/rohan/uc/sp-id/eof/png/gs/03.pbm"
-- > edit_pbm_le_all fn
edit_pbm_le_all :: FilePath -> IO ()
edit_pbm_le_all pbm_fn = do
  let gen_nm dir = replaceExtension (concat [".le.",[B.direction_char dir],".pbm"]) pbm_fn
      mk dir = edit_pbm_le dir pbm_fn (gen_nm dir)
  mapM_ mk [B.RIGHT,B.LEFT,B.DOWN,B.UP]

help :: String
help =
    unlines
    ["img-edit pbm le {r|l|d|u} input-file output-file"
    ,"img-edit pbm le/all pbm-file"
    ," pbm = portable bitmap (black & white)"
    ," le = leading edges transform"
    ," r = right, l = left, d = down, u = up"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["pbm","le",[dir],in_fn,out_fn] -> edit_pbm_le (B.parse_dir_char' dir) in_fn out_fn
    ["pbm","le/all",pbm_fn] -> edit_pbm_le_all pbm_fn
    _ -> putStrLn help
