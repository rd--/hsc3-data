-- | Load coordinates from KML file.
module Sound.SC3.Data.XML.KML where

import Data.List.Split {- split -}
import qualified Text.XML.Light as X {- xml -}

xml_element_text_cdata_uniq :: X.Element -> String
xml_element_text_cdata_uniq e =
    case X.elContent e of
      [X.Text (X.CData X.CDataText str _)] -> str
      _ -> error "element_text_cdata_uniq"

kml_2_2 :: String
kml_2_2 = "http://www.opengis.net/kml/2.2"

kml_name :: String -> X.QName
kml_name nm = X.blank_name {X.qName = nm,X.qURI = Just kml_2_2}

-- | (latitude,longitude,elevation:metres)
type KML_Coordinate = (Double,Double,Double)

kml_parse_coordinate :: String -> KML_Coordinate
kml_parse_coordinate s =
    case splitOn "," s of
      [p,q,r] -> (read p,read q,read r)
      _ -> error "kml_parse_coordinate"

kml_parse_coordinates :: String -> [KML_Coordinate]
kml_parse_coordinates = map kml_parse_coordinate . words

kml_read_coordinates :: String -> [[KML_Coordinate]]
kml_read_coordinates s =
    let p = case X.parseXMLDoc s of
              Nothing -> error "kml_read_coordinates: no parse"
              Just e -> X.findElements (kml_name "coordinates") e
    in map (kml_parse_coordinates . xml_element_text_cdata_uniq) p

kml_load_coordinates :: FilePath -> IO [[KML_Coordinate]]
kml_load_coordinates = fmap kml_read_coordinates . readFile
