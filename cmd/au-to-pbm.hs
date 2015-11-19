import qualified Data.Vector.Storable as V {- vector -}
import System.Environment {- base -}

import qualified Sound.File.NeXT as AU {- hsc3-sf -}
import qualified Sound.File.NeXT.Vector as AU {- hsc3-data -}

import Sound.SC3.Data.Bitmap.PBM {- hsc3-data -}
import Sound.SC3.Data.Bitmap.Type {- hsc3-data -}

-- > let fn = "/home/rohan/sw/hsc3-sf/au/mc-4-16.au"
-- > au_to_pbm fn (fn ++ ".pbm")
-- > pbm_print_ascii (fn ++ ".pbm")
au_to_pbm :: FilePath -> FilePath -> IO ()
au_to_pbm au_fn pbm_fn = do
  (hdr,vec) <- AU.au_read_f32_vec au_fn
  let nr = AU.channelCount hdr
      nc = AU.frameCount hdr
      dm = (nr,nc)
      f ix = let n = vec V.! ix_to_linear_co dm ix in n > 0.5
  pbm4_write pbm_fn (bitindices_to_pbm (dm,filter f (bm_indices dm)))

main :: IO ()
main = do
  a <- getArgs
  case a of
    [au_fn,pbm_fn] -> au_to_pbm au_fn pbm_fn
    _ -> putStrLn "au-to-pbm au-file pbm-file"
