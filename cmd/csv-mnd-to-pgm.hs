import System.Environment {- base -}

import qualified Music.Theory.Array.CSV.Midi.MND as C {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}
import qualified Sound.SC3 as S {- hsc3 -}
import qualified Sound.SC3.Data.Image.PGM as I {- hmt-diagrams -}

load_csv :: FilePath -> IO (T.Wseq Double (Double,Double))
load_csv = fmap C.midi_tseq_to_midi_wseq . C.midi_tseq_read

wseq_to_pgm :: (Int,Int) -> T.Wseq Double (Double,Double) -> I.PGM
wseq_to_pgm (w,h) sq =
  let (_,et) = T.wseq_tspan sq
      tm_incr = et / fromIntegral w
      tm_seq = take w [0,tm_incr ..]
      to_y mnn = h - 1 - floor (S.linlin' mnn 21 108 0 (fromIntegral h))
      to_grey = floor . (* 255) . (/ 127)
      to_entry (x,(_,(mnn,vel))) = ((to_y mnn,x),to_grey vel)
      uncollate (k,v) = zip (repeat k) v
      nd = zip [0..] (map (T.wseq_at sq) tm_seq)
      pgm = I.pgm_from_list (h,w) (concatMap (map to_entry . uncollate) nd)
  in I.pgm_invert pgm

-- > let c_fn = "/home/rohan/sw/hmt/csv/mnd/1080-C01.csv"
-- > let c_fn = "/home/rohan/uc/sp-id/csv/music/ngv/s-gyrostasis.plain.csv"
-- > csv_mnd_to_pgm (1200,200) c_fn "/tmp/t.pgm"
csv_mnd_to_pgm :: (Int,Int) -> FilePath -> FilePath -> IO ()
csv_mnd_to_pgm (w,h) csv_fn pgm_fn = do
  sq <- load_csv csv_fn
  I.pgm5_save_0 pgm_fn (wseq_to_pgm (w,h) sq)

main :: IO ()
main = do
  a <- getArgs
  case a of
    [w,h,csv_fn,pgm_fn] -> csv_mnd_to_pgm (read w,read h) csv_fn pgm_fn
    _ -> putStrLn "csv-mnd-to-pgm width height csv-file pgm-file"
