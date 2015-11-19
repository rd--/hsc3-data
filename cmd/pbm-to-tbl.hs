import System.Environment {- base -}

import qualified Music.Theory.List as T {- hmt -}

import qualified Sound.File.HSndFile as SF {- hsc3-sf-hsndfile -}
import qualified Sound.SC3.Data.Bitmap.Type as B {- hsc3-data -}
import qualified Sound.SC3.Data.Image.Plain as I {- hsc3-data -}
import qualified Sound.SC3.Lang.Collection as L {- hsc3-lang -}
import qualified Sound.SC3.Lang.Math.Statistics as L {- hsc3-lang -}

-- > img_tbl_bw_avg L.mean True "/home/rohan/uc/sp-id/eof/png/gs/02.png" "/tmp/t.au"
img_tbl_bw_avg :: Num n => ([n] -> Double) -> Bool -> FilePath -> FilePath -> IO ()
img_tbl_bw_avg avg_f nrm img_fn au_fn = do
  i <- I.img_load img_fn
  let (nc,nr) = I.img_dimensions i
      nrm_f = if nrm then L.normalise_rng (0,fromIntegral nc - 1) (0,1) else id
  print ("(w/nc,h/nr)",(nc,nr))
  let tbl = nrm_f .
            map snd .
            T.fill_gaps_ascending 0 (0,nc - 1) .
            map (fmap (avg_f . map fromIntegral)) .
            T.collate_on snd fst .
            snd .
            B.bitarray_to_bitindices .
            I.img_bw_to_bitarray $ i
  SF.write au_fn (SF.Header 1 nc 1 SF.fmt_au_f32_le) [tbl]
  return ()

help :: String
help = unlines
       ["img-to-tbl bw md nrm? image-file au-file"
       ," md = median | mean"
       ," nrm? = normalise signal (t|f)"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["bw",md,nrm,ifn,ofn] -> img_tbl_bw_avg (L.parse_averaging_f md) (nrm == "t") ifn ofn
    _ -> putStrLn help
