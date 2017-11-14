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
    ["print-parameters",fn] -> ld fn >>= wr. concatMap dx7_parameter_seq_pp . hex_voices
    ["print-voice-names",fn] -> ld fn >>= wr . map voice_name . hex_voices
    _ -> usage
