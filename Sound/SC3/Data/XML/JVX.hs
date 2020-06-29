-- | <http://javaview.de/rsrc/jvx.dtd>

module Sound.SC3.Data.XML.JVX where

import qualified Text.XML.Light as X {- xml -}

import Data.CG.Minus.Plain {- hcg-minus -}

import qualified Sound.SC3.Data.XML as XML {- hsc3-data -}

-- * GENERA

-- | JVX DOCTYPE.
jvx_doctype :: String
jvx_doctype = "jvx-model"

-- | Load jvx-model.
jvx_load :: FilePath -> IO X.Element
jvx_load = XML.xml_load_err

-- | jvx-model.title
jvx_title :: X.Element -> String
jvx_title = XML.xml_elem_text_cdata_uniq . XML.x_get_elem "title"

-- | jvx-model.geometries
jvx_geometries :: X.Element -> [X.Element]
jvx_geometries = XML.x_get_elem_set "geometries"

jvx_geometry :: X.Element -> X.Element
jvx_geometry e = case jvx_geometries e of {[g] -> g;_ -> error "jvx_geometry?"}

jvx_read_p :: X.Element -> [Double]
jvx_read_p = map read . words . XML.xml_elem_text_cdata_uniq

p_to_p3 :: [t] -> V3 t
p_to_p3 p =
  case p of
    [x,y,z] -> (x,y,z)
    _ -> error "p_to_p3"

-- | jvx-model.geometries.geometry.pointSet.points.p
jvx_points_v3 :: X.Element -> [V3 Double]
jvx_points_v3 =
  map (p_to_p3 . jvx_read_p) .
  XML.x_get_elem_set "p" .
  XML.x_get_elem_path ["geometries","geometry","pointSet","points"]

jvx_read_f :: X.Element -> [Int]
jvx_read_f = map read . words . XML.xml_elem_text_cdata_uniq

-- | jvx-model.geometries.geometry.faceSet.faces.f
jvx_faces :: X.Element -> [[Int]]
jvx_faces =
  map jvx_read_f .
  XML.x_get_elem_set "f" .
  XML.x_get_elem_path ["geometries","geometry","faceSet","faces"]

{-
fn = "/home/rohan/rd/j/2020-06-19/2013.10.001.jvx"
e <- jvx_load fn
jvx_points_v3 e
jvx_faces e
-}
