import Control.Monad {- base -}
import Data.List {- base -}
import System.Environment {- base -}
import Numeric {- base -}

import Sound.SC3.Data.LPC {- hsc3-data -}

help :: [String]
help =
  ["lpc print header file-name"
  ,"lpc print {frame|column} csv precision:int file-name frame:int"]

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

lpc_print_header :: FilePath -> IO ()
lpc_print_header fn = do
  lpc <- lpcRead fn
  let hdr = lpcHeader lpc
  putStrLn (record_pp hdr)

lpc_print_frame_csv :: Int -> FilePath -> Int -> IO ()
lpc_print_frame_csv k fn n = do
  lpc <- lpcRead fn
  let hdr = lpcHeader lpc
  when (n >= lpcNFrames hdr) (error "lpc: n > nframes")
  let frm = lpcFrames lpc !! n
  when (length frm /= lpcFrameSize hdr) (error "lpc: framesize?")
  putStrLn (intercalate "," (map (float_pp k) frm))

lpc_print_column_csv :: Int -> FilePath -> Int -> IO ()
lpc_print_column_csv k fn n = do
  lpc <- lpcRead fn
  let hdr = lpcHeader lpc
      frm = lpcFrames lpc
  when (n >= lpcFrameSize hdr) (error "lpc: n > frame_size")
  let col = transpose frm !! n
  when (length col /= lpcNFrames hdr) (error "lpc: n_frames?")
  putStrLn (intercalate "," (map (float_pp k) col))

main :: IO ()
main = do
    a <- getArgs
    case a of
      ["print","header",fn] -> lpc_print_header fn
      ["print","frame","csv",k,fn,n] -> lpc_print_frame_csv (read k) fn (read n)
      ["print","column","csv",k,fn,n] -> lpc_print_column_csv (read k) fn (read n)
      _ -> putStrLn (unlines help)

{-
fn = "/home/rohan/sw/hsc3-data/data/lpc/fate.lpc"
lpc_print_header fn
lpc_print_frame_csv 6 fn 0
lpc_print_column_csv 4 fn 3
-}
