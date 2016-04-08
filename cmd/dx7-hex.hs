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
  case a of
    ["print-parameters",fn] -> load_dx7_sysex_hex fn >>= putStrLn . unlines . concatMap dx7_parameter_pp . hex_voices
    ["print-voice-names",fn] -> load_dx7_sysex_hex fn >>= putStrLn . unlines . map voice_name . hex_voices
    _ -> usage
