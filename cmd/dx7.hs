import Control.Monad {- base -}
import System.Environment {- base -}
import System.IO {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.Byte as Byte {- hmt -}

import qualified Sound.SC3.Data.Yamaha.DX7 as DX7 {- hsc3-data -}

usage :: IO ()
usage =
    let h = ["hsc3-dx7 cmd opt"
            ,"  sysex add input-file output-file"
            ,"  sysex print [csv | hex | parameters | voice-data-list | voice-names] file-name..."
            ,"  sysex rewrite input-file output-file"
            ,"  sysex verify file-name..."]
    in putStrLn (unlines h)

dx7_sysex_print_f :: ((Int,DX7.DX7_Voice) -> String) -> [FilePath] -> IO ()
dx7_sysex_print_f op =
  let wr fn x = case x of
                  Just bnk -> putStr (unlines (map op (zip [1::Int ..] bnk)))
                  Nothing -> hPutStrLn stderr ("ERROR: dx7_sysex_print: " ++ fn)
  in mapM_ (\fn -> DX7.dx7_load_sysex_try fn >>= wr fn)

-- > let fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7/vrc/VRC-106-B.syx"
-- > dx7_sysex_print "voice-names" [fn]
-- > dx7_sysex_print "csv" [fn]
dx7_sysex_print :: String -> [FilePath] -> IO ()
dx7_sysex_print cmd fn =
  let print_csv = DX7.dx7_voice_to_csv . snd
      print_hex = Byte.byte_seq_hex_pp False . snd
      print_parameters = unlines . DX7.dx7_parameter_seq_pp . snd
      print_voice_data_list = unlines . DX7.dx7_voice_data_list_pp . snd
      print_voice_name (k,v) = printf "%2d %s" k (DX7.dx7_voice_name '?' v)
  in case cmd of
    "csv" -> dx7_sysex_print_f print_csv fn
    "hex" -> dx7_sysex_print_f print_hex fn
    "parameters" -> dx7_sysex_print_f print_parameters fn
    "voice-names" -> dx7_sysex_print_f print_voice_name fn
    "voice-data-list" -> dx7_sysex_print_f print_voice_data_list fn
    _ -> usage

dx7_sysex_verify_1 :: FilePath -> IO ()
dx7_sysex_verify_1 fn = do
  dat <- DX7.dx7_read_fmt9_sysex_err fn
  bnk <- DX7.dx7_load_fmt9_sysex_err fn
  let r = DX7.dx7_fmt9_sysex_verify 0 dat == (True,True,True,True) && DX7.dx7_bank_verify True bnk
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

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["sysex","add",fn1,fn2] -> dx7_sysex_add fn1 fn2
    "sysex":"print":cmd:fn -> dx7_sysex_print cmd fn
    ["sysex","rewrite",fn1,fn2] -> dx7_sysex_rewrite fn1 fn2
    "sysex":"verify":fn -> dx7_sysex_verify fn
    _ -> usage
