import System.Environment {- base -}

import Sound.SC3.Data.Yamaha.DX7

usage :: IO ()
usage =
    let h = ["print-parameters file-name"
            ,"print-voice-names file-name"
            ,"print-voice-data-list file-name"]
    in putStrLn (unlines h)

main :: IO ()
main = do
  a <- getArgs
  let ld = fmap dx7_bank_voices . load_dx7_sysex_hex
      wr = putStrLn . unlines
  case a of
    ["print-parameters",fn] -> ld fn >>= wr. concatMap dx7_parameter_seq_pp
    ["print-voice-names",fn] -> ld fn >>= wr . map dx7_voice_name
    ["print-voice-data-list",fn] -> ld fn >>= wr . concatMap dx7_voice_data_list_pp
    _ -> usage
