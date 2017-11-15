import System.Environment {- base -}

import Sound.SC3.Data.Yamaha.DX7

usage :: IO ()
usage =
    let h = ["print-parameters file-name"
            ,"print-voice-names file-name"]
    in putStrLn (unlines h)

main :: IO ()
main = do
  a <- getArgs
  let ld = load_dx7_sysex_hex
      wr = putStrLn . unlines
  case a of
    ["print-parameters",fn] -> ld fn >>= wr. concatMap dx7_parameter_seq_pp . dx7_bank_voices
    ["print-voice-names",fn] -> ld fn >>= wr . map dx7_voice_name . dx7_bank_voices
    _ -> usage
