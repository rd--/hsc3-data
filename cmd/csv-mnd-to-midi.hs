import System.Environment {- base -}

import qualified Sound.Sc3.Data.Midi.Plain as M {- hsc3-data -}

usage :: [String]
usage =
  ["csv-mnd-to-midi rw:bool tempo:int time-signature:int/int csv-file midi-file"
  ,""
  ,"  rw: True = rewrite to remove overlapping notes, False = no re-writing"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    [rw,tc,ts_n,ts_d,fn1,fn2] -> M.cvs_mnd_to_midi0 (read rw) (read tc) (read ts_n,read ts_d) fn1 fn2
    _ -> putStrLn (unlines usage)
