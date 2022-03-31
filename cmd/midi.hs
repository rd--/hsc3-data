import System.Environment {- base -}

import qualified Codec.Midi as C {- HCodecs -}

import qualified Music.Theory.Array.CSV as T {- hmt-base -}

import qualified Sound.Midi.Type as M {- midi-osc -}

import qualified Sound.SC3.Data.Midi.File.C as File.C {- hsc3-data -}
import qualified Sound.SC3.Data.Midi.Plain as Plain {- hsc3-data -}

node_to_text :: Int -> (Int,C.Message) -> [String]
node_to_text n (t,m) =
  let l_f = (["T","","",""] ++)
      r_f = ("M" :) . map show . (\(p,q,r) -> [fromIntegral p,q,r]) . M.cvm_to_cvm3
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

midi_to_csv_text :: FilePath -> IO ()
midi_to_csv_text m_fn = do
  m <- File.C.c_load_midi m_fn
  let tbl = concatMap (track_to_text) (zip [0..] (map C.toAbsTime (C.tracks m)))
  putStrLn (gen_csv tbl)

midi_header :: FilePath -> IO ()
midi_header m_fn = do
  m <- File.C.c_load_midi m_fn
  let (ty,t_div,n) = File.C.c_midi_header m
  print ("file-type",ty)
  print ("time-div",t_div)
  print ("number-of-tracks",n)

midi_to_csv_mnd :: FilePath -> FilePath -> IO ()
midi_to_csv_mnd midi_fn csv_fn = do
  sq <- Plain.read_midi midi_fn
  Plain.write_csv_mnd csv_fn sq

usage :: [String]
usage =
  ["hsc3-midi"
  ,""
  ,"  midi-header midi-file"
  ,"  midi-to-csv-mnd midi-file csv-file"
  ,"  midi-to-csv-text midi-file"
  ]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["midi-header",midi_fn] -> midi_header midi_fn
    ["midi-to-csv-mnd",midi_fn,csv_fn] -> midi_to_csv_mnd midi_fn csv_fn
    ["midi-to-csv-text",midi_fn] -> midi_to_csv_text midi_fn
    _ -> putStrLn (unlines usage)

{-

let midi_fn = "/home/rohan/data/midi/scarlatti/sankey/K175.MID"
midi_to_csv midi_fn
midi_header midi_fn

let midi_fn = "/home/rohan/sw/hsc3-data/data/midi/BWV-1080-1.midi"
let csv_fn = "/home/rohan/sw/hmt/data/csv/mnd/1080-C01.csv"
midi_to_csv_mnd midi_fn csv_fn

midi_to_csv_mnd "/home/rohan/data/midi/scarlatti/sankey/K175.MID" "/dev/stdout"

-}
