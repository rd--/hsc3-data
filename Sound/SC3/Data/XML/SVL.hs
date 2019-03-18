-- | SVL files are produced by <https://www.sonicvisualiser.org/>.
module Sound.SC3.Data.XML.SVL where

import Data.List {- base -}

import qualified Text.XML.Light as X {- xml -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Pitch as T {- hmt -}
import qualified Music.Theory.Pitch.Spelling as T {- hmt -}
import qualified Music.Theory.Time.Notation as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}

import qualified Sound.SC3.Data.XML as XML {- hsc3-data -}

-- * GENERA

-- | SVL DOCTYPE.
svl_doctype :: String
svl_doctype = "sonic-visualiser"

-- | The /model/ is an empty element with the following attributes.
svl_model_attr :: [String]
svl_model_attr =
  ["id","name","sampleRate","start","end","type","dimensions","resolution"
  ,"notifyOnAdd","dataset","subtype","valueQuantization","minimum","maximum","units"]

-- | The /sv.data.dataset/ element contains a sequence of /point/ elements.
svl_dataset_attr :: [String]
svl_dataset_attr = ["id","dimensions"]

-- | The /sv.data.model/ element.
svl_get_model :: X.Element -> X.Element
svl_get_model = XML.x_get_elem_path ["data","model"]

-- | Get model type and sub-type.
svl_model_type :: X.Element -> (String,Maybe String)
svl_model_type m =
    if XML.x_elem_name m /= "model"
    then error "svl_model_ty"
    else (XML.x_get_attr "type" m,X.findAttr (X.unqual "subtype") m)

-- | Get /model/ and ensure it has the indicated type and sub-type.
svl_get_model_of_type :: (String, Maybe String) -> X.Element -> X.Element
svl_get_model_of_type ty e =
    let m = svl_get_model e
    in if svl_model_type m == ty
       then m
       else error "svl_get_model_of_type"

-- | The /sv.data.dataset/ element.
svl_get_dataset :: X.Element -> X.Element
svl_get_dataset = XML.x_get_elem_path ["data","dataset"]

-- | Load the /sv/ element from an SVL file.
svl_load :: FilePath -> IO (Maybe X.Element)
svl_load = XML.xml_load

svl_load_err :: FilePath -> IO X.Element
svl_load_err = XML.xml_load_err

-- * NOTES-LAYER, TIME-INSTANTS-LAYER

-- | SR = sample rate
type SR = Double

-- | Times and durations are given as frame counts.
type FRAME = Int

svl_model_sample_rate :: X.Element -> SR
svl_model_sample_rate = read . XML.x_get_attr "sampleRate"

svl_model_dimenions :: X.Element -> Int
svl_model_dimenions = read . XML.x_get_attr "dimenions"

-- | (value,level,label)
type SVL_NOTE = (Int,Double,String)

-- | ((frame,duration),data)
type SVL_PT t = ((FRAME,FRAME),t)

-- | Parse dimensions=1=time-instant /point/ element.
svl_parse_point_d1 :: X.Element -> SVL_PT ()
svl_parse_point_d1 e = ((read (XML.x_get_attr "frame" e),0),())

-- | Parse dimensions=3=note /point/ element.
svl_parse_point_d3 :: X.Element -> SVL_PT SVL_NOTE
svl_parse_point_d3 e =
    ((read (XML.x_get_attr "frame" e)
     ,read (XML.x_get_attr "duration" e))
    ,(read (XML.x_get_attr "value" e)
     ,read (XML.x_get_attr "level" e)
     ,XML.x_get_attr "label" e))

svl_parse_sparse :: Maybe String -> (X.Element -> ((FRAME, FRAME), t)) -> X.Element -> (SR, T.Wseq FRAME t)
svl_parse_sparse st f e =
  let md = svl_get_model_of_type ("sparse",st) e
      sr = svl_model_sample_rate md
      pt = XML.x_get_elem_set "point" (svl_get_dataset e)
  in (sr,map f pt)

svl_load_sparse_note :: FilePath -> IO (SR,T.Wseq FRAME SVL_NOTE)
svl_load_sparse_note = fmap (svl_parse_sparse (Just "note") svl_parse_point_d3) . svl_load_err

svl_load_sparse_time_instant :: FilePath -> IO (SR,T.Wseq FRAME ())
svl_load_sparse_time_instant = fmap (svl_parse_sparse Nothing svl_parse_point_d1) . svl_load_err

-- * CSEC/SEC

-- | Quantise to nearest multiple of /k/.
--
-- > map (quantise 25) [-100,-90 .. 100]
-- > map (quantise 100) [-100,-50 .. 100]
quantise :: Integral a => a -> a -> a
quantise k n =
  let k2 = k `div` 2
      (d,m) = n `divMod` k
  in if m > k2 then k * (d + 1) else k * d

-- | q = quantise, sr = sample-rate, x = frame-count
--
-- > svl_frame_to_csec 50 48000 72000 == 150
svl_frame_to_csec :: Int -> SR -> Int -> T.CSEC
svl_frame_to_csec q sr x = quantise q (round (100 * fromIntegral x / sr))

svl_wseq_to_tm :: (SR -> FRAME -> t) -> (SR,T.Wseq FRAME u) -> T.Wseq t u
svl_wseq_to_tm tm_f (sr,sq) =
  let bimap1 f (t,u) = (f t,f u)
  in T.wseq_tmap (bimap1 (tm_f sr)) sq

svl_load_sparse_note_tm :: (SR -> FRAME -> t) -> FilePath -> IO (T.Wseq t SVL_NOTE)
svl_load_sparse_note_tm tm_f fn = fmap (svl_wseq_to_tm tm_f) (svl_load_sparse_note fn)

svl_load_sparse_note_csec :: Int -> FilePath -> IO (T.Wseq T.CSEC SVL_NOTE)
svl_load_sparse_note_csec q = svl_load_sparse_note_tm (svl_frame_to_csec q)

svl_frame_to_sec :: SR -> Int -> T.SEC
svl_frame_to_sec sr x = round (fromIntegral x / sr)

svl_load_sparse_note_sec :: FilePath -> IO (T.Wseq T.SEC SVL_NOTE)
svl_load_sparse_note_sec = svl_load_sparse_note_tm svl_frame_to_sec

svl_load_sparse_note_mnn_accum :: (Ord t,Num t) => (SR -> FRAME -> t) -> FilePath -> IO (Bool,T.Tseq t ([Int], [Int], [Int]))
svl_load_sparse_note_mnn_accum tm_f =
  fmap (T.wseq_begin_end_accum . T.wseq_map (\(e,_,_) -> e)) .
  svl_load_sparse_note_tm tm_f

-- * SVL_NODE

-- | (start-time,([note],[duration]))
type SVL_NODE t n = (t,([n],[t]))

svl_node_map :: ([u] -> [v]) -> SVL_NODE t u -> SVL_NODE t v
svl_node_map f (tm,(el,du)) = (tm,(f el,du))

-- * SVL_NODE_m

type SVL_NODE_m t = SVL_NODE t T.Midi

svl_load_node_m :: Ord t => (SR -> FRAME -> t) -> (Int -> Int) -> FilePath -> IO [SVL_NODE_m t]
svl_load_node_m tm_f mnn_f fn = do
  pt <- svl_load_sparse_note_tm tm_f fn
  let n = T.collate (map (\((tm,du),(mnn,_,_)) -> (tm,(mnn,du))) pt)
      to_p (tm,(mnn,du)) = (tm,(map mnn_f mnn,du))
  return (map (to_p . (\(tm,el) -> (tm,unzip el))) n)

-- * SVL_NODE_p

type SVL_NODE_p t = SVL_NODE t T.Pitch

svl_load_node_p :: Ord t => (SR -> FRAME -> t) -> (Int -> Int) -> FilePath -> IO [SVL_NODE_p t]
svl_load_node_p tm_f mnn_f =
  fmap (map (svl_node_map T.spell_midi_set)) .
  svl_load_node_m tm_f mnn_f

svl_node_p_csec_pp :: SVL_NODE_p T.CSEC -> String
svl_node_p_csec_pp (tm,(p,du)) =
  let csec_tm_pp = T.mincsec_pp_opt True . T.csec_to_mincsec
      csec_du_pp = show . flip div 100
  in unwords [csec_tm_pp tm
             ,intercalate "," (map T.pitch_pp p)
             ,intercalate "," (map csec_du_pp du)]

svl_node_p_csec_seq_wr :: [SVL_NODE_p T.CSEC] -> IO ()
svl_node_p_csec_seq_wr =  putStrLn . unlines . map svl_node_p_csec_pp
