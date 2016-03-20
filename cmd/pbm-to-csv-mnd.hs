import Data.Function {- base -}
import Data.List {- base -}
import System.Environment {- base -}

import qualified Music.Theory.Array.CSV.Midi.MND as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}

import qualified Sound.File.HSndFile as SF {- hsc3-sf-hsndfile -}

import qualified Sound.SC3.Lang.Collection as L {- hsc3-lang -}

import qualified Sound.SC3.Data.Bitmap.Type as B {- hsc3-data -}
import qualified Sound.SC3.Data.Bitmap.PBM as P {- hsc3-data -}

type R = Double
type OPT = ((R,R,Maybe FilePath,Bool,Bool) -- midi note number
           ,(R,Maybe FilePath) -- time
           ,(R,Maybe FilePath) -- duration
           ,(R,Maybe FilePath)) -- velocity

tbl_rd :: Maybe FilePath -> IO [Double]
tbl_rd fn =
    case fn of
      Nothing -> return [1]
      Just fn' -> fmap (head . snd) (SF.read fn')

-- nc = number of columns (width), nr = number of rows (height)
pbm_to_csv_mnd :: OPT -> FilePath -> FilePath -> IO ()
pbm_to_csv_mnd opt pbm_fn csv_fn = do
  let ((mnn,mnn_incr,mnn_tbl,inv,le),(tm_incr,tm_tbl),(du,du_tbl),(vel,vel_tbl)) = opt
  i <- P.read_pbm pbm_fn
  tm_mod <- tbl_rd tm_tbl
  vel_mod <- tbl_rd vel_tbl
  du_mod <- tbl_rd du_tbl
  mnn_mod <- tbl_rd mnn_tbl
  let ((nr,nc),bi') = let z = P.pbm_to_bitindices i
                      in if le then B.bitindices_leading_edges B.RIGHT z else z
      bi = sortBy (compare `on` snd) bi'
      mnn_sq = let sq = zipWith (*) [mnn,mnn + mnn_incr .. ] (L.resamp1 nr mnn_mod)
               in if inv then reverse sq else sq
      tm_sq = T.dx_d 0 (map (* tm_incr) (L.resamp1 nc tm_mod))
      vel_sq = map (* vel) (L.resamp1 nc vel_mod)
      du_sq = map (* du) (L.resamp1 nc du_mod)
      f (y,x) = ((tm_sq !! x,du_sq !! y),(mnn_sq !! y,vel_sq !! x,0))
  putStrLn (unwords . map (T.double_pp 3) $ mnn_sq)
  T.csv_mnd_write_tseq 4 csv_fn (T.midi_wseq_to_midi_tseq (map f bi))

help :: [String]
help =
    ["pbm-to-csv-mnd" ++
     " mnn mnn+ mnn~ inv le tm+ tm~ du du~ gn gn~" ++
     " pbm-file csv-file"
    ," + = increment, ~ = table (multiplier) | nil"
    ," mnn = midi note number"
    ," inv | id = frequency inverse, id = identity"
    ," le | id = leading edge"
    ," tm = time, du = duration, gn = gain"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    [mnn,mnn_incr,mnn_tbl,inv,le,tm_incr,tm_tbl,du,du_tbl,gn,gn_tbl,pbm_fn,csv_fn] ->
        let rd_tbl x = if x == "nil" then Nothing else Just x
            opt = ((read mnn,read mnn_incr,rd_tbl mnn_tbl,inv == "inv",le == "le")
                  ,(read tm_incr,rd_tbl tm_tbl)
                  ,(read du,rd_tbl du_tbl)
                  ,(read gn,rd_tbl gn_tbl))
        in pbm_to_csv_mnd opt pbm_fn csv_fn
    _ -> putStrLn (unlines help)

