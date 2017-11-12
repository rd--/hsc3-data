module Sound.SC3.Data.XML.SVL where

import Data.List {- base -}

import qualified Data.ByteString as B {- bytestring -}
import qualified Text.XML.Light as X {- xml -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Pitch as T {- hmt -}

import Sound.SC3.Data.XML

svl_doctype :: String
svl_doctype = "sonic-visualiser"

svl_model_attr :: [String]
svl_model_attr = ["id","name","sampleRate","start","end","type","dimensions","resolution","notifyOnAdd","dataset","subtype","valueQuantization","minimum","maximum","units"]

svl_dataset_attr :: [String]
svl_dataset_attr = ["id","dimensions"]

type SR = Double

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

svl_model_sample_rate :: X.Element -> SR
svl_model_sample_rate = read . x_get_attr "sampleRate"

-- | (frame,value,duration)
type SVL_PT = (Int,Int,Int)

svl_point :: X.Element -> SVL_PT
svl_point e =
    (read (x_get_attr "frame" e)
    ,read (x_get_attr "value" e)
    ,read (x_get_attr "duration" e))

frame_to_sec :: SR -> Int -> Int
frame_to_sec sr x = round (fromIntegral x / sr)

svl_pt_to_sec :: SR -> SVL_PT -> (Int,Int,Int)
svl_pt_to_sec sr (tm,mnn,du) = (frame_to_sec sr tm,mnn,frame_to_sec sr du)

svl_load :: FilePath -> IO (Maybe X.Element)
svl_load fn = do
  b <- B.readFile fn
  return (X.parseXMLDoc b)

-- * Node

-- | (start-time,([note],[dur]))
type SVL_NODE n = (Int,([n],[Int]))
type SVL_NODE_m = SVL_NODE T.Midi

svl_load_node_m :: (Int -> Int) -> FilePath -> IO [SVL_NODE_m]
svl_load_node_m mnn_f fn = do
  Just e <- svl_load fn
  let sr = svl_model_sample_rate (svl_get_model_of_type ("sparse","note") e)
      m = sort (map (svl_pt_to_sec sr . svl_point) (x_get_elem_set "point" (svl_get_dataset e)))
      n = T.collate (map (\(tm,mnn,du) -> (tm,(mnn,du))) m)
      to_p (tm,(mnn,du)) = (tm,(map mnn_f mnn,du))
  return (map (to_p . (\(tm,el) -> (tm,unzip el))) n)

