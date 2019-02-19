import Data.List {- base -}
import System.Environment {- base -}

import qualified Sound.SC3.Data.Yamaha.DX7 as DX7

usage :: IO ()
usage =
    let h = ["print-parameters file-name"
            ,"print-voice-names file-name"
            ,"print-voice-data-list file-name"]
    in putStrLn (unlines h)

main :: IO ()
main = do
  a <- getArgs
  let ld = DX7.dx7_load_sysex_hex
      wr = putStrLn . unlines
      ic = intercalate [""]
  case a of
    ["print-parameters",fn] -> ld fn >>= wr . ic . map DX7.dx7_parameter_seq_pp
    ["print-voice-names",fn] -> ld fn >>= wr . map DX7.dx7_voice_name
    ["print-voice-data-list",fn] -> ld fn >>= wr . ic . map DX7.dx7_voice_data_list_pp
    _ -> usage
