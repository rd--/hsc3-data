-- | <http://javaview.de/rsrc/jvx.dtd>
module Sound.Sc3.Data.Xml.Jvx where

import qualified Text.XML.Light as X {- xml -}

import Data.Cg.Minus.Plain {- hcg-minus -}

import qualified Sound.Sc3.Data.Xml as Xml {- hsc3-data -}

p_to_p2 :: [t] -> V2 t
p_to_p2 p =
  case p of
    [x,y] -> (x,y)
    _ -> error "p_to_p2"

p_to_p3 :: [t] -> V3 t
p_to_p3 p =
  case p of
    [x,y,z] -> (x,y,z)
    _ -> error "p_to_p3"

-- | Jvx doctype.
jvx_doctype :: String
jvx_doctype = "jvx-model"

-- | Load jvx-model.
jvx_load_model :: FilePath -> IO X.Element
jvx_load_model = Xml.xml_load_err

-- | 'jvx_title' and 'jvx_geometries' of 'jvx_load_model'
jvx_load_geom :: FilePath -> IO (String,[X.Element])
jvx_load_geom fn = do
  e <- jvx_load_model fn
  return (jvx_title e,jvx_geometries e)

-- | jvx-model.title
jvx_title :: X.Element -> String
jvx_title = Xml.xml_elem_text_cdata_uniq . Xml.x_get_elem "title"

-- | jvx-model.geometries.geometry
jvx_geometries :: X.Element -> [X.Element]
jvx_geometries = Xml.x_get_elem_set "geometry" . Xml.x_get_elem "geometries"

jvx_read_p :: X.Element -> [Double]
jvx_read_p = map read . words . Xml.xml_elem_text_cdata_uniq

-- | (jvx-model.geometries.geometry).pointSet.points.p
jvx_points_v3 :: X.Element -> [V3 Double]
jvx_points_v3 =
  map (p_to_p3 . jvx_read_p) .
  Xml.x_get_elem_set "p" .
  Xml.x_get_elem_path ["pointSet","points"]

jvx_read_f :: X.Element -> [Int]
jvx_read_f = map read . words . Xml.xml_elem_text_cdata_uniq

-- | (jvx-model.geometries.geometry).faceSet.faces.f
jvx_faces :: X.Element -> [[Int]]
jvx_faces =
  map jvx_read_f .
  Xml.x_get_elem_set "f" .
  Xml.x_get_elem_path ["faceSet","faces"]

jvx_faces_m :: X.Element -> Maybe [[Int]]
jvx_faces_m e = if Xml.x_has_elem "faceSet" e then Just (jvx_faces e) else Nothing

jvx_read_l :: X.Element -> [Int]
jvx_read_l = map read . words . Xml.xml_elem_text_cdata_uniq

-- | (jvx-model.geometries.geometry).lineSet.lines.l
jvx_lines :: X.Element -> [V2 Int]
jvx_lines =
  map (p_to_p2 . jvx_read_l) .
  Xml.x_get_elem_set "l" .
  Xml.x_get_elem_path ["lineSet","lines"]

jvx_lines_m :: X.Element -> Maybe [V2 Int]
jvx_lines_m e = if Xml.x_has_elem "lineSet" e then Just (jvx_lines e) else Nothing
