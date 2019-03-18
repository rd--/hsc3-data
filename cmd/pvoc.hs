import System.Environment {- base -}

import qualified Data.Vector.Unboxed as V {- vector -}

import qualified Sound.File.WAVE as W {- hsc3-sf -}
import qualified Sound.SC3.Data.PVOC as P {- hsc3-data -}
import qualified Sound.SC3.Plot as Plot {- hsc3-plot -}

pvoc_header :: FilePath -> IO ()
pvoc_header fn = do
  pv <- P.pvoc_load_vec_f32 fn
  let ((wave_hdr,pvoc_hdr),(_,nf,_)) = pv
  putStrLn (W.wave_fmt_16_pp wave_hdr)
  putStrLn (P.fmt_pvoc_80_pp pvoc_hdr)
  putStrLn ("NFRAMES = " ++ show nf)

pvoc_plot :: FilePath -> Int -> Int -> IO ()
pvoc_plot fn b0 b1 = do
  pv <- P.pvoc_load_vec_f32 fn
  let fmt ((a,f),i) = (f,i,a)
      gen b = let ch = 0
                  p = V.toList (P.pvoc_vec_f32_bin pv (ch,b))
              in map fmt (zip p [0..])
  Plot.plot_p3_ln (map gen [b0 .. b1])

usage :: [String]
usage =
  ["pvoc"
  ,""
  ,"  header pvoc-file:string"
  ,"  plot pvoc-file:string lhs-bin:int rhs-bin:int"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["header",fn] -> pvoc_header fn
    ["plot",fn,b0,b1] -> pvoc_plot fn (read b0) (read b1)
    _ -> error (unlines usage)

{-
let fn = "/home/rohan/uc/invisible/clarity/pvx/z.01.pvx"
pvoc_header fn
pvoc_plot fn 12 24
-}
