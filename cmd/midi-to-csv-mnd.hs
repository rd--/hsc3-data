import System.Environment {- base -}
import qualified Sound.SC3.Data.Midi.Plain as M {- hsc3-data -}

-- > let m_fn = "/home/rohan/sw/rsc3-midi/help/1080-C01.midi"
-- > let c_fn = "/home/rohan/sw/hmt/csv/mnd/1080-C01.csv"
-- > midi_to_csv_mnd m_fn c_fn
midi_to_csv_mnd :: FilePath -> FilePath -> IO ()
midi_to_csv_mnd midi_fn csv_fn = do
  sq <- M.read_midi midi_fn
  M.write_csv_mnd csv_fn sq

main :: IO ()
main = do
  a <- getArgs
  case a of
    [midi_fn,csv_fn] -> midi_to_csv_mnd midi_fn csv_fn
    _ -> putStrLn "midi-to-csv-mnd midi-file csv-file"
