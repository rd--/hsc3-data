import Control.Monad {- base -}
import Data.List {- base -}
import System.Environment {- base -}
import System.IO {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.Array.CSV as T {- hmt -}
import qualified Music.Theory.Byte as T {- hmt -}

import qualified Sound.SC3.Data.Yamaha.DX7 as DX7 {- hsc3-data -}
import qualified Sound.SC3.Data.Yamaha.DX7.DB as DX7 {- hsc3-data -}
import qualified Sound.SC3.Data.Yamaha.DX7.Hash as DX7 {- hsc3-data -}
import qualified Sound.SC3.Data.Yamaha.DX7.PP as DX7 {- hsc3-data -}

usage_str :: [String]
usage_str =
  ["hsc3-dx7 cmd opt"
  ,"  {hex | sysex} print print-cmd file-name..."
  ,"  sysex add input-file output-file"
  ,"  sysex rewrite input-file output-file"
  ,"  sysex verify file-name..."
  ,""
  ,"  print-cmd = csv | hash-hex | hash-names | hex | names | parameters | voice-data-list"
  ]

usage :: IO ()
usage = putStrLn (unlines usage_str)

type LD_F = FilePath -> IO (Maybe [DX7.DX7_Voice])

dx7_print_f :: LD_F -> ((Int,DX7.DX7_Voice) -> String) -> [FilePath] -> IO ()
dx7_print_f ld op =
  let wr fn x = case x of
                  Just bnk -> putStr (unlines (map op (zip [1::Int ..] bnk)))
                  Nothing -> hPutStrLn stderr ("ERROR: dx7_sysex_print: " ++ fn)
  in mapM_ (\fn -> ld fn >>= wr fn)

-- > let fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7/vrc/VRC-106-B.syx"
-- > dx7_sysex_print "names" [fn]
-- > dx7_sysex_print "hash-names" [fn]
dx7_print :: LD_F -> String -> [FilePath] -> IO ()
dx7_print ld cmd fn =
  let print_csv = DX7.dx7_voice_to_csv . snd
      print_hex pr_h (_,v) =
        if pr_h
        then intercalate "," (DX7.dx7_hash_vc_param_csv (DX7.dx7_hash_vc v))
        else T.byte_seq_hex_pp False v
      print_parameters = unlines . DX7.dx7_parameter_seq_pp . snd
      print_voice_data_list = unlines . DX7.dx7_voice_data_list_pp . snd
      print_voice_name pr_h (k,v) =
        let nm = DX7.dx7_voice_name '?' v
        in if pr_h
           then let h = DX7.dx7_voice_hash v
                in printf "%s,%s" (DX7.dx7_hash_pp h) (T.csv_quote_if_req nm)
           else printf "%2d %s" k nm
      print_f f = dx7_print_f ld f fn
  in case cmd of
    "csv" -> print_f print_csv
    "hash-hex" -> print_f (print_hex True)
    "hash-names" -> print_f (print_voice_name True)
    "hex" -> print_f (print_hex False)
    "names" -> print_f (print_voice_name False)
    "parameters" -> print_f print_parameters
    "voice-data-list" -> print_f print_voice_data_list
    _ -> usage

dx7_hex_print :: String -> [FilePath] -> IO ()
dx7_hex_print = dx7_print (fmap Just . DX7.dx7_load_hex)

dx7_sysex_print :: String -> [FilePath] -> IO ()
dx7_sysex_print = dx7_print DX7.dx7_load_sysex_try

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
    "hex":"print":cmd:fn -> dx7_hex_print cmd fn
    ["sysex","add",fn1,fn2] -> dx7_sysex_add fn1 fn2
    "sysex":"print":cmd:fn -> dx7_sysex_print cmd fn
    ["sysex","rewrite",fn1,fn2] -> dx7_sysex_rewrite fn1 fn2
    "sysex":"verify":fn -> dx7_sysex_verify fn
    _ -> usage
