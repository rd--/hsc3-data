import Control.Monad {- base -}
import Data.List {- base -}
import System.Environment {- base -}
import Numeric {- base -}

import qualified Sound.Sc3.Data.LPC as LPC {- hsc3-data -}

help :: [String]
help =
  ["lpc txt|le|be print header file-name"
  ,"lpc txt|le|be print {frame|column} csv precision:int file-name frame:int"]

record_pp :: Show r => r -> String
record_pp =
  let f c = case c of
              '{' -> "{\n  "
              ',' -> ",\n "
              '}' -> "\n}"
              _ -> [c]
  in concatMap f . show

float_pp :: RealFloat a => Int -> a -> String
float_pp k n = showFFloat (Just k) n ""

type READER = FilePath -> IO LPC.LPC


lpc_print_header :: READER -> FilePath -> IO ()
lpc_print_header reader fn = do
  lpc <- reader fn
  let hdr = LPC.lpcHeader lpc
  putStrLn (record_pp hdr)

lpc_print_frame_csv :: READER -> Int -> FilePath -> Int -> IO ()
lpc_print_frame_csv reader k fn n = do
  lpc <- reader fn
  let hdr = LPC.lpcHeader lpc
  when (n >= LPC.lpcNFrames hdr) (error "lpc: n > nframes")
  let frm = LPC.lpcFrames lpc !! n
  when (length frm /= LPC.lpcFrameSize hdr) (error "lpc: framesize?")
  putStrLn (intercalate "," (map (float_pp k) frm))

lpc_print_column_csv :: READER -> Int -> FilePath -> Int -> IO ()
lpc_print_column_csv reader k fn n = do
  lpc <- reader fn
  let hdr = LPC.lpcHeader lpc
      frm = LPC.lpcFrames lpc
  when (n >= LPC.lpcFrameSize hdr) (error "lpc: n > frame_size")
  let col = transpose frm !! n
  when (length col /= LPC.lpcNFrames hdr) (error "lpc: n_frames?")
  putStrLn (intercalate "," (map (float_pp k) col))

typ_to_reader :: String -> READER
typ_to_reader typ =
  case typ of
    "txt" -> LPC.lpc_read_text
    "le" -> LPC.lpc_read_binary LPC.LittleEndian
    "be" -> LPC.lpc_read_binary LPC.BigEndian
    _ -> error "unknown typ?"

main :: IO ()
main = do
    a <- getArgs
    case a of
      [typ,"print","header",fn] -> lpc_print_header (typ_to_reader typ) fn
      [typ,"print","frame","csv",k,fn,n] -> lpc_print_frame_csv (typ_to_reader typ) (read k) fn (read n)
      [typ,"print","column","csv",k,fn,n] -> lpc_print_column_csv (typ_to_reader typ) (read k) fn (read n)
      _ -> putStrLn (unlines help)

{-
fn = "/home/rohan/sw/hsc3-data/data/lpc/fate.lpc"
lpc_print_header (typ_to_reader "be") fn
lpc_print_frame_csv (typ_to_reader "be") 6 fn 0
lpc_print_column_csv (typ_to_reader "be") 4 fn 3
-}
