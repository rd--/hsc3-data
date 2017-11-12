module Sound.SC3.Data.XML.SVL where

import qualified Data.ByteString as B {- bytestring -}
import qualified Text.XML.Light as X {- xml -}

import Sound.SC3.Data.XML

-- * SVL

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
type PT = (Int,Int,Int)

svl_point :: X.Element -> PT
svl_point e =
    (read (x_get_attr "frame" e)
    ,read (x_get_attr "value" e)
    ,read (x_get_attr "duration" e))

frame_to_sec :: SR -> Int -> Int
frame_to_sec sr x = round (fromIntegral x / sr)

pt_to_sec :: SR -> PT -> (Int,Int,Int)
pt_to_sec sr (tm,mnn,du) = (frame_to_sec sr tm,mnn,frame_to_sec sr du)

-- > Just e <- svl_load (L.prj_file "svl/flax-A_fcd.svl")
-- > x_elem_name e == "sv"
-- > svl_model_sample_rate (svl_get_model_of_type ("sparse","note") e)
svl_load :: FilePath -> IO (Maybe X.Element)
svl_load fn = do
  b <- B.readFile fn
  return (X.parseXMLDoc b)

