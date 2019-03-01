import System.Environment {- base -}

import qualified Sound.SC3.Data.Midi.Plain as M {- hsc3-data -}

main :: IO ()
main = do
  a <- getArgs
  case a of
    [tc,ts_n,ts_d,fn1,fn2] -> M.cvs_mnd_to_midi0 (read tc) (read ts_n,read ts_d) fn1 fn2
    _ -> putStrLn "csv-mnd-to-midi tempo:int time-signature:int/int csv-file midi-file"
