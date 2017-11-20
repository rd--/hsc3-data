module Sound.SC3.Data.XML.SVL where

import Data.List {- base -}

import qualified Data.ByteString as B {- bytestring -}
import qualified Text.XML.Light as X {- xml -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Pitch as T {- hmt -}
import qualified Music.Theory.Pitch.Spelling as T {- hmt -}
import qualified Music.Theory.Time.Notation as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}

import qualified Sound.SC3.Data.XML as XML {- hsc3-data -}

-- * GENERA

svl_doctype :: String
svl_doctype = "sonic-visualiser"

svl_model_attr :: [String]
svl_model_attr =
  ["id","name","sampleRate","start","end","type","dimensions","resolution"
  ,"notifyOnAdd","dataset","subtype","valueQuantization","minimum","maximum","units"]

svl_dataset_attr :: [String]
svl_dataset_attr = ["id","dimensions"]

svl_get_model :: X.Element -> X.Element
svl_get_model = XML.x_get_elem_path ["data","model"]

-- | Get model type and sub-type.
svl_model_type :: X.Element -> (String,String)
svl_model_type m =
    if XML.x_elem_name m /= "model"
    then error "svl_model_ty"
    else (XML.x_get_attr "type" m,XML.x_get_attr "subtype" m)

svl_get_model_of_type :: (String, String) -> X.Element -> X.Element
svl_get_model_of_type ty e =
    let m = svl_get_model e
    in if svl_model_type m == ty
       then m
       else error "svl_get_model_of_type"

svl_get_dataset :: X.Element -> X.Element
svl_get_dataset = XML.x_get_elem_path ["data","dataset"]

svl_load :: FilePath -> IO (Maybe X.Element)
svl_load fn = do
  b <- B.readFile fn
  return (X.parseXMLDoc b)

-- | SN = SPARSE-NOTE

-- | SR = sample rate
type SR = Double

type FRAME = Int

svl_model_sample_rate :: X.Element -> SR
svl_model_sample_rate = read . XML.x_get_attr "sampleRate"

-- | ((frame,duration),(value,level,label))
type SVL_SN_PT_DATA = (Int,Double,String)
type SVL_SN_PT = ((FRAME,FRAME),SVL_SN_PT_DATA)

svl_parse_point :: X.Element -> SVL_SN_PT
svl_parse_point e =
    ((read (XML.x_get_attr "frame" e)
     ,read (XML.x_get_attr "duration" e))
    ,(read (XML.x_get_attr "value" e)
     ,read (XML.x_get_attr "level" e)
     ,XML.x_get_attr "label" e))

svl_load_sparse_note :: FilePath -> IO (SR,T.Wseq FRAME SVL_SN_PT_DATA)
svl_load_sparse_note fn = do
  Just e <- svl_load fn
  let md = svl_get_model_of_type ("sparse","note") e
      sr = svl_model_sample_rate md
      pt = XML.x_get_elem_set "point" (svl_get_dataset e)
  return (sr,map svl_parse_point pt)

-- * CSEC/SEC

type CSEC = Int

-- | Quantise to nearest multiple of /k/.
--
-- > map (quantise 25) [-100,-90 .. 100]
-- > map (quantise 100) [-100,-50 .. 100]
quantise :: Integral a => a -> a -> a
quantise k n =
  let k2 = k `div` 2
      (d,m) = n `divMod` k
  in if m > k2 then k * (d + 1) else k * d

svl_frame_to_csec :: Int -> SR -> Int -> CSEC
svl_frame_to_csec q sr x = quantise q (round (100 * fromIntegral x / sr))

svl_load_sparse_note_tm :: (SR -> FRAME -> t) -> FilePath -> IO (T.Wseq t SVL_SN_PT_DATA)
svl_load_sparse_note_tm tm_f fn = do
  (sr,sq) <- svl_load_sparse_note fn
  let bimap1 f (t,u) = (f t,f u)
  return (T.wseq_tmap (bimap1 (tm_f sr)) sq)

svl_load_sparse_note_csec :: Int -> FilePath -> IO (T.Wseq CSEC SVL_SN_PT_DATA)
svl_load_sparse_note_csec q = svl_load_sparse_note_tm (svl_frame_to_csec q)

type SEC = Int

svl_frame_to_sec :: SR -> Int -> SEC
svl_frame_to_sec sr x = round (fromIntegral x / sr)

svl_load_sparse_note_sec :: FilePath -> IO (T.Wseq SEC SVL_SN_PT_DATA)
svl_load_sparse_note_sec = svl_load_sparse_note_tm svl_frame_to_sec

svl_load_sparse_note_mnn_accum :: (Ord t,Num t) => (SR -> FRAME -> t) -> FilePath -> IO (Bool,T.Tseq t ([Int], [Int], [Int]))
svl_load_sparse_note_mnn_accum tm_f fn = do
  sq <- svl_load_sparse_note_tm tm_f fn
  let sel_mnn (mnn,_,_) = mnn
      mnn_sq = T.wseq_map sel_mnn sq
      ol = T.wseq_has_overlaps (==) mnn_sq
      mnn_sq_edit = if ol then T.wseq_remove_overlaps (==) id mnn_sq else mnn_sq
      a_sq = T.tseq_begin_end_accum (T.tseq_group (T.wseq_begin_end mnn_sq_edit))
  return (ol,a_sq)

-- * SVL_NODE

-- | (start-time,([note],[dur]))
type SVL_NODE n = (CSEC,([n],[CSEC]))

svl_node_map :: ([t] -> [u]) -> SVL_NODE t -> SVL_NODE u
svl_node_map f (tm,(el,du)) = (tm,(f el,du))

-- * SVL_NODE_m

type SVL_NODE_m = SVL_NODE T.Midi

svl_load_node_m :: Int -> (Int -> Int) -> FilePath -> IO [SVL_NODE_m]
svl_load_node_m q mnn_f fn = do
  pt <- svl_load_sparse_note_csec q fn
  let n = T.collate (map (\((tm,du),(mnn,_,_)) -> (tm,(mnn,du))) pt)
      to_p (tm,(mnn,du)) = (tm,(map mnn_f mnn,du))
  return (map (to_p . (\(tm,el) -> (tm,unzip el))) n)

-- * SVL_NODE_p

type SVL_NODE_p = SVL_NODE T.Pitch

svl_load_node_p :: Int -> (Int -> Int) -> FilePath -> IO [SVL_NODE_p]
svl_load_node_p q mnn_f = fmap (map (svl_node_map T.spell_midi_set)) . svl_load_node_m q mnn_f

svl_node_p_pp :: SVL_NODE_p -> String
svl_node_p_pp (tm,(p,du)) =
  let csec_tm_pp = T.mincsec_pp_opt True . T.csec_to_mincsec
      csec_du_pp = show . flip div 100
  in unwords [csec_tm_pp tm
             ,intercalate "," (map T.pitch_pp p)
             ,intercalate "," (map csec_du_pp du)]

svl_node_p_seq_wr :: [SVL_NODE_p] -> IO ()
svl_node_p_seq_wr =  putStrLn . unlines . map svl_node_p_pp
