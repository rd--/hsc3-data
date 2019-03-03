import Control.Monad {- base -}
import Data.List {- base -}
import System.Environment {- base -}
import System.IO {- base -}

import qualified Music.Theory.Byte as Byte {- hmt -}

import qualified Sound.SC3.Data.Yamaha.DX7 as DX7 {- hsc3-data -}

dx7_sysex_print :: ([DX7.DX7_Voice] -> [String]) -> [FilePath] -> IO ()
dx7_sysex_print op =
  let wr fn x = case x of
                  Just bnk -> putStr (unlines (op bnk))
                  Nothing -> hPutStrLn stderr ("ERROR: dx7_sysex_print: " ++ fn)
  in mapM_ (\fn -> DX7.dx7_load_sysex_try fn >>= wr fn)

dx7_sysex_verify_1 :: FilePath -> IO ()
dx7_sysex_verify_1 fn = do
  dat <- DX7.dx7_read_fmt9_sysex fn
  bnk <- DX7.dx7_load_fmt9_sysex fn
  let r = DX7.dx7_fmt9_sysex_verify 0 dat == (True,True,True,True) && DX7.dx7_bank_verify bnk
  putStrLn (if r then "TRUE" else "FALSE: " ++ show (dat,bnk))

dx7_sysex_verify :: [FilePath] -> IO ()
dx7_sysex_verify = mapM_ dx7_sysex_verify_1

dx7_sysex_add :: FilePath -> FilePath -> IO ()
dx7_sysex_add fn1 fn2 = do
  dat <- DX7.dx7_read_u8 fn1
  when (length dat /= 4096) (error "dx7_sysex_add: NOT 4096")
  DX7.dx7_write_fmt9_sysex fn2 (DX7.dx7_fmt9_sysex_gen 0 dat)

dx7_sysex_rewrite :: FilePath -> FilePath -> IO ()
dx7_sysex_rewrite fn1 fn2 = do
  src <- DX7.dx7_read_u8 fn1 -- ie. do not verify
  let dat = DX7.dx7_fmt9_sysex_dat src
  when (length dat /= 4096) (error "dx7_sysex_rewrite?")
  DX7.dx7_write_fmt9_sysex fn2 (DX7.dx7_fmt9_sysex_gen 0 dat)

usage :: IO ()
usage =
    let h = ["hsc3-dx7 cmd opt"
            ,"  sysex add input-file output-file"
            ,"  sysex print [hex | parameters | voice-data-list | voice-names] file-name..."
            ,"  sysex rewrite input-file output-file"
            ,"  sysex verify file-name..."]
    in putStrLn (unlines h)

main :: IO ()
main = do
  a <- getArgs
  let ic = intercalate [""]
      print_hex = map Byte.byte_seq_hex_pp
      print_parameters = ic . map DX7.dx7_parameter_seq_pp
      print_voice_names = map DX7.dx7_voice_name
      print_voice_data_list = ic . map DX7.dx7_voice_data_list_pp
  case a of
    ["sysex","add",fn1,fn2] -> dx7_sysex_add fn1 fn2
    "sysex":"print":"hex":fn -> dx7_sysex_print print_hex fn
    "sysex":"print":"parameters":fn -> dx7_sysex_print print_parameters fn
    "sysex":"print":"voice-names":fn -> dx7_sysex_print print_voice_names fn
    "sysex":"print":"voice-data-list":fn -> dx7_sysex_print print_voice_data_list fn
    ["sysex","rewrite",fn1,fn2] -> dx7_sysex_rewrite fn1 fn2
    "sysex":"verify":fn -> dx7_sysex_verify fn
    _ -> usage
