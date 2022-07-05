import System.Environment {- base -}

import qualified Music.Theory.List as T {- hmt-base -}

import qualified Music.Theory.Array.Csv.Midi.Mnd as C {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}

import qualified Sound.Sc3.Data.Image.Pgm as I {- hmt-diagrams -}

import qualified Sound.Sc3 as S {- hsc3 -}

load_csv :: FilePath -> IO (T.Wseq Double (C.Event Double))
load_csv = fmap C.midi_tseq_to_midi_wseq . C.csv_mnd_read_tseq

wseq_to_pgm :: (Int,Int) -> T.Wseq Double (C.Event Double) -> I.Pgm
wseq_to_pgm (w,h) sq =
  let (_,et) = T.wseq_tspan sq
      tm_incr = et / fromIntegral w
      tm_seq = take w (T.adj2 1 [0,tm_incr ..])
      nd = zip [0..] (map (T.wseq_at_window sq) tm_seq)
      to_y mnn = h - 1 - floor (S.linlin_ma S.sc3_mul_add mnn 21 108 0 (fromIntegral h))
      to_grey = floor . (* 255) . (/ 127)
      to_entry (x,(_,(mnn,vel,_,_))) = ((to_y mnn,x),to_grey vel)
      uncollate (k,v) = zip (repeat k) v
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
