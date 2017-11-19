module Sound.SC3.Data.XML.SVL where

import Data.List {- base -}

import qualified Data.ByteString as B {- bytestring -}
import qualified Text.XML.Light as X {- xml -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Pitch as T {- hmt -}
import qualified Music.Theory.Pitch.Spelling as T {- hmt -}
import qualified Music.Theory.Time.Notation as T {- hmt -}

import Sound.SC3.Data.XML

svl_doctype :: String
svl_doctype = "sonic-visualiser"

svl_model_attr :: [String]
svl_model_attr =
  ["id","name","sampleRate","start","end","type","dimensions","resolution"
  ,"notifyOnAdd","dataset","subtype","valueQuantization","minimum","maximum","units"]

svl_dataset_attr :: [String]
svl_dataset_attr = ["id","dimensions"]

svl_get_model :: X.Element -> X.Element
svl_get_model = x_get_elem_path ["data","model"]

svl_model_type :: X.Element -> (String,String)
svl_model_type m =
    if x_elem_name m /= "model"
    then error "svl_get_model_ty"
    else (x_get_attr "type" m,x_get_attr "subtype" m)

svl_get_model_of_type :: (String, String) -> X.Element -> X.Element
svl_get_model_of_type ty e =
    let m = svl_get_model e
    in if svl_model_type m == ty
       then m
       else error "svl_get_model_of_type"

svl_get_dataset :: X.Element -> X.Element
svl_get_dataset = x_get_elem_path ["data","dataset"]

-- | (frame,value,duration)
type SVL_PT = (Int,Int,Int)

svl_point :: X.Element -> SVL_PT
svl_point e =
    (read (x_get_attr "frame" e)
    ,read (x_get_attr "value" e)
    ,read (x_get_attr "duration" e))

svl_load :: FilePath -> IO (Maybe X.Element)
svl_load fn = do
  b <- B.readFile fn
  return (X.parseXMLDoc b)

-- * SVL_NODE

type CSEC = Int

-- | (start-time,([note],[dur]))
type SVL_NODE n = (CSEC,([n],[CSEC]))

svl_node_map :: ([t] -> [u]) -> SVL_NODE t -> SVL_NODE u
svl_node_map f (tm,(el,du)) = (tm,(f el,du))

-- * SVL_NODE_m

type SVL_NODE_m = SVL_NODE T.Midi

-- | SR = sample rate
type SR = Double

-- > map (quantise 25) [-100,-90 .. 100]
-- > map (quantise 100) [-100,-50 .. 100]
quantise :: Integral a => a -> a -> a
quantise k n =
  let k2 = k `div` 2
      (d,m) = n `divMod` k
  in if m > k2 then k * (d + 1) else k * d

svl_model_sample_rate :: X.Element -> SR
svl_model_sample_rate = read . x_get_attr "sampleRate"

frame_to_csec :: SR -> Int -> Int -> Int
frame_to_csec sr q x = quantise q (round (100 * fromIntegral x / sr))

svl_pt_to_csec :: SR -> Int -> SVL_PT -> (CSEC,Int,CSEC)
svl_pt_to_csec sr q (tm,mnn,du) = (frame_to_csec sr q tm,mnn,frame_to_csec sr q du)

svl_load_node_m :: Int -> (Int -> Int) -> FilePath -> IO [SVL_NODE_m]
svl_load_node_m q mnn_f fn = do
  Just e <- svl_load fn
  let sr = svl_model_sample_rate (svl_get_model_of_type ("sparse","note") e)
      pt = x_get_elem_set "point" (svl_get_dataset e)
      m = sort (map (svl_pt_to_csec sr q . svl_point) pt)
      n = T.collate (map (\(tm,mnn,du) -> (tm,(mnn,du))) m)
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
