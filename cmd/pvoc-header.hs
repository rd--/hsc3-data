import System.Environment {- base -}

import qualified Sound.File.WAVE as W {- hsc3-sf -}
import qualified Sound.SC3.Data.PVOC as P {- hsc3-data -}

-- > pvoc_header "/home/rohan/data/audio/pf-c5.pvx"
pvoc_header :: FilePath -> IO ()
pvoc_header fn = do
  pv <- P.pvoc_load_vec_f32 fn
  let ((wave_hdr,pvoc_hdr),(_,nf,_)) = pv
  putStrLn$ W.wave_fmt_16_pp wave_hdr
  putStrLn$ P.fmt_pvoc_80_pp pvoc_hdr
  putStrLn$ "NFRAMES = " ++ show nf

main :: IO ()
main = do
  a <- getArgs
  case a of
    [fn] -> pvoc_header fn
    _ -> error "pvoc-header pvoc-file"
