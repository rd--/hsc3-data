import System.Environment {- base -}
import qualified Sound.SC3.Data.Midi.Plain as M {- hsc3-data -}

-- > midi_to_csv_mnd "/home/rohan/sw/rsc3-midi/help/1080-C01.midi" "/dev/stdout"
midi_to_csv_mnd :: FilePath -> FilePath -> IO ()
midi_to_csv_mnd ifn ofn = do
  sq <- M.read_midi ifn
  M.write_csv ofn sq

main :: IO ()
main = do
  a <- getArgs
  case a of
    [ifn,ofn] -> midi_to_csv_mnd ifn ofn
    _ -> putStrLn "hsc3-midi-to-csv-mnd input-file output-file"
