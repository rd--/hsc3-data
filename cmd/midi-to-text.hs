import System.Environment {- base -}

import qualified Codec.Midi as C {- HCodecs -}

import qualified Music.Theory.Array.CSV as T {- hmt -}

import qualified Sound.Midi.Type as M {- midi-osc -}

filetype :: C.FileType -> Int
filetype ty =
    case ty of
      C.SingleTrack -> 0
      C.MultiTrack -> 1
      C.MultiPattern -> 2

timediv :: C.TimeDiv -> Int
timediv td =
    case td of
      C.TicksPerBeat i -> i
      _ -> error "timediv"

parse_c_message :: C.Message -> Either [String] (M.Midi_Message Int)
parse_c_message c =
    let to_w8 = fromIntegral
    in case c of
         C.NoteOn ch mnn vel -> Right (M.Note_On (to_w8 ch) mnn vel)
         C.NoteOff ch mnn vel -> Right (M.Note_Off (to_w8 ch) mnn vel)
         C.ProgramChange ch pc -> Right (M.Program_Change (to_w8 ch) pc)
         C.ControlChange ch i j -> Right (M.Control_Change (to_w8 ch) i j)
         C.TrackName nm -> Left ["track-name",nm]
         C.TempoChange tm -> Left ["tempo-change",show tm]
         C.TrackEnd -> Left ["track-end"]
         C.TimeSignature b0 b1 b2 b3 -> Left ("time-signature" : map show [b0,b1,b2,b3])
         C.KeySignature b0 b1 -> Left ("key-signature" : map show [b0,b1])
         C.SMPTEOffset b0 b1 b2 b3 b4 -> Left ("smpte-offset" : map show [b0,b1,b2,b3,b4])
         _ -> Left ["unrecognised"]

node_to_text :: Int -> (Int,C.Message) -> [String]
node_to_text n (t,m) =
    let to_w8 = fromIntegral
        l_f = (["T","","",""] ++)
        r_f = ("M" :) . map show . (\(p,q,r) -> [p,to_w8 q,to_w8 r]) . M.m_encode_cvm3
    in show n : show t : either l_f r_f (parse_c_message m)

track_to_text :: (Int, [(Int, C.Message)]) -> [[String]]
track_to_text (n,t) = map (node_to_text n) t

pad_right :: a -> Int -> [a] -> [a]
pad_right x n l = l ++ replicate (n - length l) x

gen_csv :: [[String]] -> String
gen_csv tbl =
    let w = maximum (map length tbl)
        tbl' = map (pad_right "" w) tbl
    in T.csv_table_pp id (False,',',False,T.CSV_Align_Right) (Nothing,tbl')

load_midi :: FilePath -> IO C.Midi
load_midi m_fn = do
  r <- C.importFile m_fn
  return (either (error "midi_to_text: read failed") id r)

-- > let m_fn = "/home/rohan/sw/rsc3-midi/help/1080-C01.midi" in midi_to_csv m_fn
midi_to_csv :: FilePath -> IO ()
midi_to_csv m_fn = do
  m <- load_midi m_fn
  let tbl = concatMap (track_to_text) (zip [0..] (map C.toAbsTime (C.tracks m)))
  putStrLn (gen_csv tbl)

midi_header :: FilePath -> IO ()
midi_header m_fn = do
  m <- load_midi m_fn
  print ("file-type",filetype (C.fileType m))
  print ("time-div",timediv (C.timeDiv m))
  print ("number-of-tracks",length (C.tracks m))

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["header",m_fn] -> midi_header m_fn
    ["csv",m_fn] -> midi_to_csv m_fn
    _ -> putStrLn "midi-to-text {header | csv} midi-file"
