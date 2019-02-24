import Data.List {- base -}
import System.Environment {- base -}

import qualified Sound.SC3.Data.Yamaha.DX7 as DX7 {- hsc3-data -}

usage :: IO ()
usage =
    let h = ["hex|sysex print parameters file-name"
            ,"hex|sysex print voice-names file-name"
            ,"hex|sysex print voice-data-list file-name"
            ,"sysex verify file-name"
            ,"sysex rewrite input-file output-file"]
    in putStrLn (unlines h)

dx7_hex_print :: FilePath -> (DX7.DX7_Bank -> [String]) -> IO ()
dx7_hex_print fn op =
  let wr = putStrLn . unlines
  in DX7.dx7_load_sysex_hex fn >>= wr . op

dx7_sysex_print :: FilePath -> (DX7.DX7_Bank -> [String]) -> IO ()
dx7_sysex_print fn op =
  let wr = putStrLn . unlines
  in DX7.dx7_load_sysex fn >>= wr . op

dx7_sysex_verify :: FilePath -> IO ()
dx7_sysex_verify fn = do
  dat <- DX7.dx7_load_sysex_u8 fn
  bnk <- DX7.dx7_load_sysex fn
  let r = DX7.dx7_sysex_u8_verify dat == (True,True,True,True) && DX7.dx7_bank_verify bnk
  putStrLn (if r then "TRUE" else "FALSE: " ++ show (dat,bnk))

dx7_sysex_rewrite :: FilePath -> FilePath -> IO ()
dx7_sysex_rewrite fn1 fn2 = do
  src <- DX7.dx7_load_sysex_u8 fn1
  let dat = take 4096 (drop 6 src)
      dst = DX7.dx7_sysex_fmt_9_hdr ++ dat ++ [DX7.dx7_checksum dat,0xF7]
  DX7.dx7_store_sysex_u8 fn2 dst

main :: IO ()
main = do
  a <- getArgs
  let ic = intercalate [""]
      print_parameters = ic . map DX7.dx7_parameter_seq_pp
      print_voice_names = map DX7.dx7_voice_name
      print_voice_data_list = ic . map DX7.dx7_voice_data_list_pp
  case a of
    ["hex","print","parameters",fn] -> dx7_hex_print fn print_parameters
    ["hex","print","voice-names",fn] -> dx7_hex_print fn print_voice_names
    ["hex","print","voice-data-list",fn] -> dx7_hex_print fn print_voice_data_list
    ["sysex","print","parameters",fn] -> dx7_sysex_print fn print_parameters
    ["sysex","print","voice-names",fn] -> dx7_sysex_print fn print_voice_names
    ["sysex","print","voice-data-list",fn] -> dx7_sysex_print fn print_voice_data_list
    ["sysex","verify",fn] -> dx7_sysex_verify fn
    ["sysex","rewrite",fn1,fn2] -> dx7_sysex_rewrite fn1 fn2
    _ -> usage
