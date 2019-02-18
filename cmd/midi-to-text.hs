import Data.Maybe {- base -}
import System.Environment {- base -}

import qualified Codec.Midi as C {- HCodecs -}

import qualified Music.Theory.Array.CSV as T {- hmt -}
import qualified Music.Theory.Math.Convert as T {- hmt -}

import qualified Sound.Midi.Type as M {- midi-osc -}

import qualified Sound.SC3.Data.Midi.File.C as File.C {- hsc3-data -}

node_to_text :: Int -> (Int,C.Message) -> [String]
node_to_text n (t,m) =
  let to_w8 = T.int_to_word8
      l_f = (["T","","",""] ++)
      r_f = ("M" :) . map show . (\(p,q,r) -> [p,to_w8 q,to_w8 r]) . fromMaybe (error "node_to_text") . M.mm_to_cvm3
    in show n : show t : either l_f r_f (File.C.c_parse_message m)

track_to_text :: (Int, [(Int, C.Message)]) -> [[String]]
track_to_text (n,t) = map (node_to_text n) t

pad_right :: a -> Int -> [a] -> [a]
pad_right x n l = l ++ replicate (n - length l) x

gen_csv :: [[String]] -> String
gen_csv tbl =
  let w = maximum (map length tbl)
      tbl' = map (pad_right "" w) tbl
  in T.csv_table_pp id (False,',',False,T.CSV_Align_Right) (Nothing,tbl')

-- > fn = "/home/rohan/sw/hsc3-data/data/midi/BWV-1080-1.midi"
-- > midi_to_csv fn
midi_to_csv :: FilePath -> IO ()
midi_to_csv m_fn = do
  m <- File.C.c_load_midi m_fn
  let tbl = concatMap (track_to_text) (zip [0..] (map C.toAbsTime (C.tracks m)))
  putStrLn (gen_csv tbl)

-- > midi_header fn
midi_header :: FilePath -> IO ()
midi_header m_fn = do
  m <- File.C.c_load_midi m_fn
  let (ty,t_div,n) = File.C.c_midi_header m
  print ("file-type",ty)
  print ("time-div",t_div)
  print ("number-of-tracks",n)

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["header",m_fn] -> midi_header m_fn
    ["csv",m_fn] -> midi_to_csv m_fn
    _ -> putStrLn "midi-to-text {header | csv} midi-file"
