import System.Environment {- base -}

import qualified Music.Theory.List as T {- hmt-base -}

import Sound.Sc3.Common.Buffer {- hsc3 -}

import qualified Sound.File.HSndFile as Sf {- hsc3-sf-hsndfile -}

import qualified Sound.Sc3.Data.Bitmap.Pbm as P {- hsc3-data -}
import qualified Sound.Sc3.Lang.Math.Statistics as L {- hsc3-lang -}

-- > pbm_to_tbl L.mean True "/home/rohan/uc/sp-id/eof/pbm/gs/02.pbm" "/tmp/t.au"
pbm_to_tbl :: Num n => ([n] -> Double) -> Bool -> FilePath -> FilePath -> IO ()
pbm_to_tbl avg_f nrm pbm_fn au_fn = do
  i <- P.read_pbm pbm_fn
  let (nc,nr) = P.pbm_dimensions i
      nrm_f = if nrm then normalise_rng (0,fromIntegral nc - 1) (0,1) else id
  print ("(w/nc,h/nr)",(nc,nr))
  let tbl = nrm_f .
            map snd .
            T.fill_gaps_ascending 0 (0,nc - 1) .
            map (fmap (avg_f . map fromIntegral)) .
            T.collate_on snd fst .
            snd .
            P.pbm_to_bitindices $ i
  Sf.write au_fn (Sf.Sf_Header 1 nc 1 Sf.fmt_au_f32_be) [tbl]
  return ()

help :: String
help = unlines
       ["pbm-to-tbl md nrm? image-file au-file"
       ," md = median | mean"
       ," nrm? = normalise signal (t|f)"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    [md,nrm,pbm_fn,au_fn] -> pbm_to_tbl (L.parse_averaging_f md) (nrm == "t") pbm_fn au_fn
    _ -> putStrLn help
